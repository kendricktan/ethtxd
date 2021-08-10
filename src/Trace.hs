{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}

{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}



module Trace where

import           EVM                              (Env (..), FrameContext (..),
                                                   Log (..), Trace (..),
                                                   TraceData (..), VM (..))
import           EVM.Concrete                     (createAddress)
import           EVM.Dapp                         (DappContext (..), emptyDapp)
import           EVM.Format                       (formatSBinary, showError)
import           EVM.Symbolic                     (len, litAddr, litWord)
import           EVM.Transaction                  (Transaction (..),
                                                   txAccessMap)
import           EVM.Types                        (Buffer (..), SymWord (..),
                                                   hexByteString, num, strip0x,
                                                   w256lit)

import           Numeric                          (showHex)

import           Control.Lens.Combinators         (assign, view)

import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.State.Strict (StateT (..), execStateT, get,
                                                   put)

import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   Value (..), defaultOptions,
                                                   genericToEncoding, object,
                                                   (.=))
import           Data.Maybe                       (fromMaybe, isNothing)
import           Data.SBV                         (WordN (..), literal,
                                                   unliteral)

import           Data.Text                        (Text, pack, unpack)
import           Data.Tree                        (Forest (..), Tree (..))
import           Fetch                            (Tx (..))
import           GHC.Generics                     (Generic)

import qualified Data.HashMap.Lazy                as HML
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map

import qualified Data.Text                        as T
import qualified Data.Tree.Zipper                 as Zipper


import qualified EVM
import qualified EVM.FeeSchedule                  as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Stepper
import qualified EVM.VMTest                       as VMTest

mergeA :: [Value] -> Value
mergeA = Object . HML.unions . map (\(Object x) -> x)

-- | Custom Data
data TxTrace = TxTrace TraceData [TxTrace] deriving (Generic, Show)

instance ToJSON TxTrace where
  toJSON (TxTrace x []) = toJSON x
  toJSON (TxTrace x xs) =
    let a = toJSON x
        b = object ["internal" .= toJSON xs]
    in mergeA [a, b]

deriving instance FromJSON TxTrace

-- | Symbolic words of 256 bits, don't show the
--   "insightful" information
showEvalSymWord :: SymWord -> String
showEvalSymWord (S _ s) = case unliteral s of
  Just x  -> ("0x" ++) $ showHex x ""
  Nothing -> "0x"

decipher = hexByteString "bytes" . strip0x

instance ToJSON TraceData where
  toJSON (EventTrace (Log addr b t)) = object [
      "type" .= String "event"
    , "address" .= show addr
    , "bytes" .= formatSBinary b
    , "data" .= map (pack . showEvalSymWord) t ]

  toJSON (FrameTrace (CreationContext addr _ _ _)) = object ["type" .= String "create" , "address" .= show addr]
  toJSON (FrameTrace (CallContext target context _ _ _ _ calldata _ _)) =
    let target' = show target
        calldata' = formatSBinary calldata
        fType = if target == context
          then "call"
          else "delegateCall"
    in object [
        "type" .= String fType
      , "address" .= target'
      , "calldata" .= calldata'
    ]

  toJSON (ErrorTrace e)    = object ["type" .= String "error", "value" .= show e]
  toJSON (ReturnTrace b _) = object ["type" .= String "return", "value" .= formatSBinary b]
  toJSON _ = Null

instance FromJSON TraceData where
  parseJSON = error "Not implemented"


vmFromTx :: Text -> Tx -> IO (EVM.VM)
vmFromTx url (Tx blockNum timestamp from to gas value nonce input) = do
  let toAddr = fromMaybe (createAddress from nonce) to
      isCreate = isNothing to
      calldata = ConcreteBuffer $ decipher input
  -- Contract we're interacting with
  contract <-
    case isCreate
      -- Creating new contract
          of
      True -> return $ EVM.initialContract (EVM.InitCode $ ConcreteBuffer input)
      -- Not creating new contract, fetch it from external source
      False ->
        EVM.Fetch.fetchContractFrom (EVM.Fetch.BlockNumber blockNum) url toAddr >>= \case
          Nothing -> error $ "No contract found at " <> show to
          Just c -> return c
  -- Return initialized VM
  return $
    VMTest.initTx $
    EVM.makeVm $
    EVM.VMOpts
      { EVM.vmoptContract = contract
      , EVM.vmoptCalldata = (calldata, litWord (num $ len calldata))
      , EVM.vmoptValue = w256lit value
      , EVM.vmoptAddress = toAddr
      , EVM.vmoptCaller = litAddr from
      , EVM.vmoptOrigin = 0
      , EVM.vmoptGas = gas
      , EVM.vmoptGaslimit = 0xffffffffffffffff
      , EVM.vmoptCoinbase = 0
      , EVM.vmoptNumber = blockNum
      , EVM.vmoptTimestamp = timestamp
      , EVM.vmoptBlockGaslimit = 0
      , EVM.vmoptGasprice = 0
      , EVM.vmoptMaxCodeSize = 0xffffffffffffffff
      , EVM.vmoptDifficulty = 0
      , EVM.vmoptSchedule = FeeSchedule.berlin
      , EVM.vmoptChainId = 1
      , EVM.vmoptCreate = isCreate
      , EVM.vmoptStorageModel = EVM.ConcreteS
      , EVM.vmoptTxAccessList = mempty
      , EVM.vmoptAllowFFI = False
      }

execTxs :: [Tx] -> Text -> EVM.Fetch.Fetcher -> StateT EVM.VM IO (Either EVM.Error Buffer)
execTxs [] _ fetcher = EVM.Stepper.interpret fetcher $ EVM.Stepper.execFully
execTxs (x:xs) url fetcher = do
  -- Inteprets previous transaction
  EVM.Stepper.interpret fetcher $ EVM.Stepper.execFully
  -- Loads new tx into it
  -- Resets previous tx state, stack frames, and result
  -- Prepares VM for tx
  vm <- get
  -- Get new VM with tx state loaded into it
  newVM <- liftIO $ vmFromTx url x
  -- But keep existing contract storage state
  let env1  = _env vm
      env2  = _env newVM
      newEnv = env1 { _contracts = Map.union (_contracts env1) (_contracts env2)}
  -- Reset VM state, trace, but keep the state
  put vm {
      _result = Nothing
    , _frames = []
    , _traces = Zipper.fromForest []
    , _state = _state newVM
    , _block = _block newVM
    , _tx = _tx newVM
    , _env = newEnv
  }
  -- Executes next state
  execTxs xs url fetcher

runVM :: Text -> EVM.Fetch.BlockNumber -> EVM.VM -> IO EVM.VM
runVM url blockNum =
  let fetcher = EVM.Fetch.http blockNum url
   in execStateT (EVM.Stepper.interpret fetcher . void $ EVM.Stepper.execFully)

encodeTx :: Tx -> TraceData
encodeTx (Tx blockNum timestamp from to gas value nonce input) =
  let toAddr = fromMaybe (createAddress from nonce) to
      isCreate = isNothing to
      calldata = ConcreteBuffer $ decipher input
      emptySubstrate = EVM.SubState mempty mempty mempty mempty mempty
  in if isCreate 
    then FrameTrace $ CreationContext toAddr 0 Map.empty emptySubstrate
    -- Context is the address itself to simulate a "call"
    else FrameTrace $ CallContext toAddr toAddr 0 0 0 Nothing calldata Map.empty emptySubstrate

encodeTree :: Tree EVM.Trace -> Tree TraceData
encodeTree (Node n ns) = (Node (_traceData n) (encodeTree <$> ns))

encodeTrace :: Tree TraceData -> TxTrace
encodeTrace (Node s ns) = TxTrace s $ encodeTrace <$> ns
