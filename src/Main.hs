{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Exception                    (SomeException, catch)
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

import           Fetch                                (fetchTx)
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
  tx <- fetchTx ethRpcUrl txHash
  case tx of
    Nothing ->
      return $
      ErrorResponse
        txHash
        "Unable to retrieve txHash (An achival node might fix this)."
    Just tx' -> do
      vm <- vmFromTx ethRpcUrl tx'
      vm' <- runVM ethRpcUrl tx' vm
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
  summary "ethtxd - Lightweight Ethereum Transaction Decoder API Service v0.1.2"

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
      txTrace <-
        liftIO $
        (getTxTrace (pack $ rpc ethtxdOpts) txHash) `catch`
        rpcExceptionHandler txHash
      json txTrace
      case txTrace of
        SuccessResponse _ _ -> status ok200
        ErrorResponse _ _   -> status status400
