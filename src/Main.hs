{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Exception                    (SomeException, catch)
import           Control.Monad                        (foldM)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       defaultOptions,
                                                       genericToEncoding,
                                                       object, (.=))
import           Data.Text                            (Text, pack)
import           GHC.Generics                         (Generic (..))
import           Network.HTTP.Types.Status            (ok200, status400)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Console.CmdArgs               (Data, Typeable, cmdArgs,
                                                       help, summary, (&=))
import           Web.Scotty                           (get, json, middleware,
                                                       param, scotty, status)

import           Fetch                                (Tx (..),
                                                       fetchPriorTxsInSameBlock,
                                                       fetchTx)
import           Trace                                (TxTrace, encodeTree,
                                                       formatForest, runVM,
                                                       vmFromTx)

import qualified EVM

data Response = SuccessResponse
    { txHash :: Text
    , traces :: [TxTrace]
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
  tx <- fetchTx ethRpcUrl txHash
  -- Get the prior transactions in the block
  priorTxHashes <- fetchPriorTxsInSameBlock ethRpcUrl txHash
  priorTxs <- sequence $ (fetchTx ethRpcUrl) <$> priorTxHashes
  case tx of
    Nothing ->
      return $
      ErrorResponse
        txHash
        "Unable to retrieve txHash (An achival node might fix this)."
    Just tx' -> do
      -- Tx data is the same, however the initial state
      -- will be the state before all the other tx's
      let tx'' = tx' { _blockNum = (_blockNum tx') - 1 }
      -- Prepare the VM
      vm <- vmFromTx ethRpcUrl $ tx''
      -- Run all the prior txs
      -- vm' <- foldM (\v t -> case t of
      --   Just t' -> runVM ethRpcUrl t' v
      --   Nothing -> return v) vm priorTxs
      -- Finally, run the selected the transaction
      vm' <- runVM ethRpcUrl tx'' vm
      -- Get the traces
      let traces = formatForest <$> (encodeTree <$> EVM.traceForest vm')
      return $ SuccessResponse txHash traces

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
