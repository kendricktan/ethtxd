{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Exception                    (SomeException, catch)
import           Control.Monad                        (foldM, void)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.State.Strict     (execStateT)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       defaultOptions,
                                                       genericToEncoding,
                                                       object, (.=))
import           Data.Text                            (Text, pack)
import           EVM.Fetch                            (BlockNumber (..), http)
import           EVM.Stepper                          (execFully, interpret)
import           GHC.Generics                         (Generic (..))
import           Network.HTTP.Types.Status            (ok200, status400)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Console.CmdArgs               (Data, Typeable, cmdArgs,
                                                       help, summary, (&=))
import           Web.Scotty                           (get, json, middleware,
                                                       param, scotty, status)

import           Data.Tree                            (Forest (..))
import           EVM                                  (Trace (..),
                                                       TraceData (..))
import           Fetch                                (Tx (..),
                                                       fetchPriorTxsInSameBlock,
                                                       fetchTx)
import           Trace                                (TxTrace(..), encodeTx, encodeTrace, encodeTree, execTxs, runVM, vmFromTx)

import qualified EVM

data Response = SuccessResponse
    { txHash :: Text
    , trace  :: TxTrace
    }
    | ErrorResponse
    { txHash :: Text
    , reason :: Text
    }
    deriving (Generic)

instance FromJSON Response

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

data EthTxdOpts = EthTxdOpts
    { port :: Int
    , rpc  :: String
    }
    deriving (Show, Data, Typeable)

getTxTrace :: Text -> Text -> IO Response
getTxTrace ethRpcUrl txHash = do
  -- Get the tx data for the current (debugging tx)
  targetTx <- fetchTx ethRpcUrl txHash
  -- Get the prior transactions in the block
  priorTxHashes <- fetchPriorTxsInSameBlock ethRpcUrl txHash
  priorTxs <- sequence $ (fetchTx ethRpcUrl) <$> priorTxHashes
  case sequence $ priorTxs <> [targetTx] of
    Nothing ->
      return $
      ErrorResponse
        txHash
        "Unable to retrieve txHash (An achival node might fix this)."
    Just txs -> do
      -- Tx data is the same, however the initial state
      -- will be the state before all the other tx's
      let firstTx  = head txs
          lastTx   = last txs
          blockNo  = EVM.Fetch.BlockNumber $ _blockNum lastTx - 1
          fetcher  = EVM.Fetch.http blockNo ethRpcUrl
          vmSeq    = execTxs (tail txs) ethRpcUrl fetcher
      -- Prepare VM for first Tx
      vm  <- vmFromTx ethRpcUrl firstTx
      -- Execute rest of the tx
      vm' <- execStateT
              (execTxs (tail txs) ethRpcUrl fetcher)
              vm
      -- Get the traces
      let txTrace = (encodeTrace . encodeTree) <$> EVM.traceForest vm'
          txTrace' = TxTrace (encodeTx lastTx) txTrace
      return $ SuccessResponse txHash txTrace'

defaultOpts =
  EthTxdOpts
    { port = 3000 &= help "Web server port (default: 3000)"
    , rpc =
        "http://localhost:8545" &=
        help
          "Ethereum RPC URL to retrieve remote state (default: http://localhost:8545)"
    } &=
  summary "ethtxd - Lightweight Ethereum Transaction Decoder API Service v0.2.0"

rpcExceptionHandler :: Text -> SomeException -> IO Response
rpcExceptionHandler txHash _ =
  return $ ErrorResponse txHash "Unable to connect to RPC Node or state is stale (might need archival node)."

main :: IO ()
main = do
  ethtxdOpts <- cmdArgs defaultOpts
  putStrLn $ "Ethereum RPC URL: " <> rpc ethtxdOpts
  scotty (port ethtxdOpts) $ do
    middleware logStdout
    get "/" $ do
      json $ object [("status" :: Text) .= ("ok" :: Text)]
      status ok200
    get "/tx/:txHash" $ do
      txHash <- param "txHash"
      trace <-
        liftIO $
        (getTxTrace (pack $ rpc ethtxdOpts) txHash) `catch`
        rpcExceptionHandler txHash
      json trace
      case trace of
        SuccessResponse _ _ -> status ok200
        ErrorResponse _ _   -> status status400
