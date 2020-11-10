{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..),
                                                       defaultOptions,
                                                       genericToEncoding)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, pack, unpack)
import           GHC.Generics                         (Generic (..))
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Environment                   (lookupEnv)
import           System.Exit                          (ExitCode (ExitFailure),
                                                       exitSuccess, exitWith)

import           System.IO.Unsafe                     (unsafePerformIO)

import           Network.HTTP.Types.Status            (ok200, status400)
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

ethRpcUrl :: Text
ethRpcUrl =
  pack $
  fromMaybe "http://localhost:8545" (unsafePerformIO $ lookupEnv "ETH_RPC_URL")

getTxTrace :: Text -> IO Response
getTxTrace txHash = do
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

main :: IO ()
main = do
  putStrLn $ "ETH_RPC_URL: " <> (unpack ethRpcUrl)
  scotty 3000 $ do
    middleware logStdout
    get "/tx/:txHash" $ do
      txHash <- param "txHash"
      txTrace <- liftIO $ getTxTrace txHash
      json txTrace
      case txTrace of
        SuccessResponse _ _ -> status ok200
        ErrorResponse _ _   -> status status400
