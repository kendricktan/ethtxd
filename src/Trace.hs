{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ImplicitParams     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE StandaloneDeriving #-}


module Trace where

import           EVM                              (Env (..), FrameContext (..),
                                                   Log (..), TraceData (..),
                                                   VM (..))
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
                                                   defaultOptions,
                                                   genericToEncoding)
import           Data.Maybe                       (fromMaybe, isNothing)
import           Data.SBV                         (literal, unliteral)
import           Data.Text                        (Text, pack, unpack)
import           Data.Tree                        (Tree (..))
import           Fetch                            (Tx (..))
import           GHC.Generics                     (Generic)

import qualified Data.Map                         as Map

import qualified Data.Text                        as T
import qualified Data.Tree.Zipper                 as Zipper


import qualified EVM
import qualified EVM.FeeSchedule                  as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Stepper
import qualified EVM.VMTest                       as VMTest


data TxTrace = TxCall
    { callTarget   :: Text
    , callSigBytes :: Text
    , callData     :: Text
    , callTrace    :: [TxTrace]
    }
    | TxDelegateCall
    { delegateCallTarget   :: Text
    , delegateCallSigBytes :: Text
    , delegateCallData     :: Text
    , delegateCallTrace    :: [TxTrace]
    }
    | TxEvent
    { eventBytes  :: Text
    , eventTopics :: [Text]
    }
    | TxReturn
    { returnData :: Text
    }
    | TxRevert
    { revertReason :: Text
    }
    deriving (Generic)

instance ToJSON TxTrace where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TxTrace

decipher = hexByteString "bytes" . strip0x

-- | Symbolic words of 256 bits, don't show the
--   "insightful" information
showEvalSymWord :: SymWord -> String
showEvalSymWord (S _ s) = case unliteral s of
  Just x  -> ("0x" ++) $ showHex x ""
  Nothing -> "0x"

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
    , _state = EVM.blankState
    , _env = newEnv
  }
  -- Executes next state
  execTxs xs url fetcher


runVM :: Text -> Tx -> EVM.VM -> IO EVM.VM
runVM url (Tx blockNum _ _ _ _ _ _ _) =
  let fetcher = EVM.Fetch.http (EVM.Fetch.BlockNumber $ blockNum) url
   in execStateT (EVM.Stepper.interpret fetcher . void $ EVM.Stepper.execFully)

encodeTrace :: EVM.Trace -> Maybe TxTrace
encodeTrace t =
  case view EVM.traceData t
    -- Event Emitted
        of
    EventTrace (Log _ bytes topics) ->
      let topics' = (map (pack . showEvalSymWord) topics)
          bytes' = formatSBinary bytes
       in Just $ TxEvent bytes' topics'
    -- Contract call
    FrameTrace (CallContext target context _ _ _ _ calldata _ _) ->
      let target' = pack (show target)
          calldata' = unpack $ formatSBinary calldata
          sig' = pack $ take 10 calldata'
          data' = pack $ drop 10 calldata'
          data'' =
            if T.length data' > 0
              then "0x" <> data'
              else data'
       in case target == context
        -- Call
                of
            True  -> return $ TxCall target' sig' data'' mempty
        -- Delegate call
            False -> return $ TxDelegateCall target' sig' data'' mempty
    -- Return Data
    ReturnTrace out (CallContext {}) -> Just $ TxReturn $ formatSBinary out
    -- Revert
    ErrorTrace e ->
      case e of
        EVM.Revert out -> do
          -- Some Dapp context, but since this is a webapp
          -- we can just construct it as an empty DappContext
          let ?context = DappContext emptyDapp mempty
          return $ TxRevert (showError out)
        _              -> return $ TxReturn (pack . show $ e)
    -- Unimportant stuff
    _ -> Nothing

encodeTree :: Tree EVM.Trace -> Tree (Maybe TxTrace)
encodeTree (Node n ns) = (Node (encodeTrace n) (encodeTree <$> ns))

formatForest :: Tree (Maybe TxTrace) -> TxTrace
formatForest (Node Nothing _) = TxReturn "0x"
formatForest (Node (Just n) ns) =
  case n of
    TxCall _ _ _ _         -> n {callTrace = formatForest <$> ns}
    TxDelegateCall _ _ _ _ -> n {delegateCallTrace = formatForest <$> ns}
    _                      -> n
