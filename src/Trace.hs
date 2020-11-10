{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Trace where

import           EVM                              (FrameContext (..), Log (..),
                                                   TraceData (..))
import           EVM.Concrete                     (createAddress)
import           EVM.Format                       (formatSBinary, showError)
import           EVM.Symbolic                     (len, litAddr, w256lit)
import           EVM.Types                        (Buffer (ConcreteBuffer),
                                                   hexByteString, num, strip0x)

import           Control.Lens.Combinators         (view)
import           Control.Monad                    (void)
import           Control.Monad.Trans.State.Strict (execStateT)
import           Data.Aeson                       (FromJSON (..), ToJSON (..),
                                                   defaultOptions,
                                                   genericToEncoding)
import           Data.Maybe                       (fromMaybe, isNothing)
import           Data.SBV                         (literal)
import           Data.Text                        (Text, pack, unpack)
import           Data.Tree                        (Tree (..))
import           Fetch                            (Tx (..))
import           GHC.Generics                     (Generic)

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
      True -> return $ EVM.initialContract (EVM.InitCode $ decipher input)
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
      , EVM.vmoptCalldata = (calldata, literal . num $ len calldata)
      , EVM.vmoptValue = w256lit value
      , EVM.vmoptAddress = toAddr
      , EVM.vmoptCaller = litAddr from
      , EVM.vmoptOrigin = 0
      , EVM.vmoptGas = gas
      , EVM.vmoptGaslimit = 0xffffffff
      , EVM.vmoptCoinbase = 0
      , EVM.vmoptNumber = blockNum
      , EVM.vmoptTimestamp = timestamp
      , EVM.vmoptBlockGaslimit = 0
      , EVM.vmoptGasprice = 0
      , EVM.vmoptMaxCodeSize = 0xffffffff
      , EVM.vmoptDifficulty = 0
      , EVM.vmoptSchedule = FeeSchedule.istanbul
      , EVM.vmoptChainId = 1
      , EVM.vmoptCreate = isCreate
      , EVM.vmoptStorageModel = EVM.ConcreteS
      }

runVM :: Text -> Tx -> EVM.VM -> IO EVM.VM
runVM url (Tx blockNum _ _ _ _ _ _ _) =
  let fetcher = EVM.Fetch.http (EVM.Fetch.BlockNumber $ blockNum - 1) url
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
            True  -> return $ TxCall target' sig' data' []
        -- Delegate call
            False -> return $ TxDelegateCall target' sig' data' []
    -- Return Data
    ReturnTrace out (CallContext {}) -> Just $ TxReturn $ formatSBinary out
    -- Revert
    ErrorTrace e ->
      case e of
        EVM.Revert out -> return $ TxRevert (showError out)
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
