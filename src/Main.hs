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
import           Data.ByteString                      (ByteString)
import           Data.Map.Strict                      (Map (..), fromList,
                                                       mapKeys, mapWithKey,
                                                       toList)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, pack)
import           Data.Text.Encoding                   (encodeUtf8)
import           EVM.Fetch                            (BlockNumber (..),
                                                       fetchContractWithSession,
                                                       http)
import           EVM.Stepper                          (execFully, interpret)
import           GHC.Generics                         (Generic (..))
import           Network.HTTP.Types.Status            (ok200, status400)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.Console.CmdArgs               (Data, Typeable, cmdArgs,
                                                       help, summary, (&=))
import           Web.Scotty                           (ActionM (..), get, json,
                                                       jsonData, middleware,
                                                       param, post, scotty,
                                                       status)

import           Data.Tree                            (Forest (..))
import           EVM                                  (Contract (..),
                                                       ContractCode (..),
                                                       Env (..), Trace (..),
                                                       TraceData (..), VM (..),
                                                       initialContract)
import           EVM.Fetch                            (readText)
import           EVM.Types                            (Addr, Buffer (..),
                                                       hexByteString, strip0x)
import           Fetch                                (Tx (..),
                                                       fetchPriorTxsInSameBlock,
                                                       fetchTx)
import           Trace                                (TxTrace (..), decipher,
                                                       encodeTrace, encodeTree,
                                                       encodeTx, execTxs, runVM,
                                                       setVmContractCode,
                                                       vmFromTx)

import qualified Data.Map.Strict                      as MS
import qualified EVM
import qualified Network.Wreq.Session                 as Session

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

data Request = Request {
  code :: Map Text Text -- Hashmap of Key: Address, Value: ByteString
} deriving (Generic)

instance FromJSON Request

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions

data EthTxdOpts = EthTxdOpts
    { port :: Int
    , rpc  :: String
    }
    deriving (Show, Data, Typeable)

getTxTrace :: Map Addr Buffer -> Text -> Text -> IO Response
getTxTrace m ethRpcUrl txHash = do
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
      -- Prepare the contract env
      m' <- sequence $ (\(k, a) -> do
                                    c <- Session.newAPISession >>= (fetchContractWithSession blockNo ethRpcUrl k)
                                    let cc = (EVM.RuntimeCode a)
                                    let ic = initialContract cc
                                    case c of
                                      -- Grab the existing nonce and balance
                                      Just c' -> return (k, ic
                                                            { _balance = _balance c'
                                                            , EVM._nonce = EVM._nonce c'
                                                            , _external = True
                                                            })
                                      -- Otherwise just create initial contract
                                      Nothing -> return (k, ic)
                        ) <$> toList m
      let m'' = fromList m'
      -- Prepare VM for first Tx
      vm <- vmFromTx ethRpcUrl firstTx
      -- Execute rest of the tx
      vm' <- execStateT
              (execTxs (tail txs) m'' ethRpcUrl fetcher)
              -- But overwrite specific contract code
              (setVmContractCode m'' vm)
      -- GG
      -- liftIO $ case (MS.lookup (readText "0x6b175474e89094c44da98b954eedeac495271d0f" :: Addr) $ _contracts $ _env (setVmContractCode m'' vm)) of
      --             Just x -> print $ _contractcode x
      --             Nothing -> print "Not found!"
      -- -- Get contract code
      -- liftIO $ case (MS.lookup (readText "0x6b175474e89094c44da98b954eedeac495271d0f" :: Addr) $ _contracts $ _env vm') of
      --             Just x -> print $ _contractcode x
      --             Nothing -> print "Not found!"
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
rpcExceptionHandler txHash e =
  return $ ErrorResponse txHash $ (pack . show) e --"Unable to connect to RPC Node or state is stale (might need archival node)."

main :: IO ()
main = do
  ethtxdOpts <- cmdArgs defaultOpts
  putStrLn $ "Ethereum RPC URL: " <> rpc ethtxdOpts

  scotty (port ethtxdOpts) $ do
    middleware logStdout
    get "/" $ do
      json $ object [("status" :: Text) .= ("ok" :: Text)]
      status ok200
    post "/tx/:txHash" $ do
      txHash <- param "txHash"
      contractsCode <- jsonData :: ActionM Request
      let contractsCode'  = mapKeys (readText :: Text -> Addr) $ code contractsCode
          contractsCode'' = (\a -> (ConcreteBuffer $ decipher $ encodeUtf8 a)) <$> contractsCode'
      trace <-
        liftIO $
        (getTxTrace contractsCode'' (pack $ rpc ethtxdOpts) txHash) `catch`
        rpcExceptionHandler txHash
      json trace
      case trace of
        SuccessResponse _ _ -> status ok200
        ErrorResponse _ _   -> status status400
