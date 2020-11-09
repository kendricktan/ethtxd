{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           EVM                              (FrameContext (..),
                                                   FrameState (..), Log (..),
                                                   TraceData (..))
import           EVM.Dapp                         (DappInfo, dappInfo)
import           EVM.Format                       (formatSBinary, showTraceTree)
import           EVM.Solidity                     (SourceCache (..))
import           EVM.Symbolic                     (forceLitBytes, len, litAddr,
                                                   w256lit)
import           EVM.Types

import           Control.Arrow                    ((>>>))
import           Control.Lens.Combinators         (view)
import           Control.Monad                    (void)
import           Control.Monad.Trans.State.Strict
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   decode, defaultOptions,
                                                   encode, genericToEncoding)
import           Data.Monoid                      (mempty)
import           Data.SBV                         (literal)
import           Data.Text                        (Text, pack, unpack)
import           Data.Text.IO                     (hPutStr)
import           Data.Tree                        (Forest (..), Tree (..),
                                                   levels)
import           Fetch
import           GHC.Generics
import           System.IO                        (stderr)

import qualified EVM
import qualified EVM.FeeSchedule                  as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Stepper
import qualified EVM.VMTest                       as VMTest
import qualified Network.Wreq.Session             as Session

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
    deriving (Generic)

instance ToJSON TxTrace where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TxTrace

txhash = "0x44c6f3f304a1e566c14063066f440d82eb7adbae57a3b9bcb1aec7c7dab65766"

emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty (SourceCache mempty mempty mempty mempty)

url = "http://localhost:8545"

codeType = EVM.RuntimeCode

decipher = hexByteString "bytes" . strip0x

-- value = 0
miner = 0

-- timestamp = 1604743
diff = 0

vmFromTx :: Tx -> IO (EVM.VM)
vmFromTx (Tx blockNum timestamp from to gas value input) = do
  contract <-
    EVM.Fetch.fetchContractFrom (EVM.Fetch.BlockNumber blockNum) url to >>= \case
      Nothing -> error $ "No contract found at " <> show to
      Just c -> return c
  calldata <- return $ ConcreteBuffer $ decipher input

  return $
    VMTest.initTx $
    EVM.makeVm $
    EVM.VMOpts
      { EVM.vmoptContract = contract
      , EVM.vmoptCalldata = (calldata, literal . num $ len calldata)
      , EVM.vmoptValue = w256lit value
      , EVM.vmoptAddress = to
      , EVM.vmoptCaller = litAddr from
      , EVM.vmoptOrigin = 0
      , EVM.vmoptGas = gas
      , EVM.vmoptGaslimit = 0xffffffff
      , EVM.vmoptCoinbase = miner
      , EVM.vmoptNumber = blockNum
      , EVM.vmoptTimestamp = timestamp
      , EVM.vmoptBlockGaslimit = 0
      , EVM.vmoptGasprice = 0
      , EVM.vmoptMaxCodeSize = 0xffffffff
      , EVM.vmoptDifficulty = diff
      , EVM.vmoptSchedule = FeeSchedule.istanbul
      , EVM.vmoptChainId = 1
      , EVM.vmoptCreate = False
      , EVM.vmoptStorageModel = EVM.ConcreteS
      }

runVM :: Tx -> EVM.VM -> IO EVM.VM
runVM (Tx blockNum _ _ _ _ _ _) =
  let block = EVM.Fetch.BlockNumber blockNum
      fetcher = EVM.Fetch.http block url
   in execStateT (EVM.Stepper.interpret fetcher . void $ EVM.Stepper.execFully)

encodeTrace :: EVM.Trace -> Maybe TxTrace
encodeTrace t =
  case view EVM.traceData t
    -- Event Emitted
        of
    EventTrace (Log _ bytes topics) ->
      let topics' = (map (pack . show) topics)
          bytes' = formatSBinary bytes
       in Just $ TxEvent bytes' topics'
    -- Contract call
    FrameTrace (CallContext target context _ _ _ _ calldata _ _) ->
      let target' = pack (show target)
          calldata' = unpack $ formatSBinary calldata
          sig' = pack $ take 10 calldata'
          data' = pack $ drop 10 calldata'
       in case target == context
        -- Call
                of
            True  -> Just $ TxCall target' sig' data' []
        -- Delegate call
            False -> Just $ TxDelegateCall target' sig' data' []
    -- Return Data
    ReturnTrace out (CallContext {}) -> Just $ TxReturn $ formatSBinary out
    -- Unimportant stuff
    _ -> Nothing

encodeTree :: Tree EVM.Trace -> Tree (Maybe TxTrace)
encodeTree (Node n ns) = (Node (encodeTrace n) (encodeTree <$> ns))

formatForest :: Tree (Maybe TxTrace) -> TxTrace
formatForest (Node Nothing _) = TxReturn "0x"
formatForest (Node (Just n) ns) =
  case n of
    TxCall _ _ _ _         -> n {callTrace = formatForest <$> ns}
    TxDelegateCall _ _ _ _ -> n {callTrace = formatForest <$> ns}
    _                      -> n

main :: IO ()
main = do
  print $ "tx hash " <> txhash
  tx <- fetchTx url txhash
  tx' <-
    case tx of
      Nothing  -> error $ "Invalid tx hash " <> (unpack txhash)
      Just tx' -> return tx'
  vm <- vmFromTx tx'
  vm' <- runVM tx' vm
  -- let traces = formatForest <$> (encodeTree <$> EVM.traceForest vm')
  -- print $ encode traces
  hPutStr stderr (showTraceTree emptyDapp vm')
  case view EVM.result vm' of
    Nothing -> error "internal error; no EVM result"
    Just (EVM.VMFailure (EVM.Revert msg)) -> print $ ByteStringS msg
    Just (EVM.VMFailure err) -> print err
    Just (EVM.VMSuccess buf) -> do
      let msg =
            case buf of
              SymbolicBuffer msg' -> forceLitBytes msg'
              ConcreteBuffer msg' -> msg'
      print $ ByteStringS msg
  return ()
