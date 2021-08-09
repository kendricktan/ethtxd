{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import           Control.Lens             ((^..), (^?))
import           Control.Lens.Combinators (ifiltered)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value (..))
import           Data.Aeson.Lens          (AsValue, key, values, _String)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text (..), pack)
import           Data.Text.Encoding       (encodeUtf8)
import           EVM.Fetch                (BlockNumber (..), fetchBlockFrom,
                                           fetchWithSession, readText, rpc,
                                           toRPC)
import           EVM.Types                (Addr, SymWord, W256)

import qualified EVM                      as EVM
import qualified Network.Wreq.Session     as Session

data Tx = Tx
    { _blockNum  :: W256
    , _timestamp :: SymWord
    , _from      :: Addr
    , _to        :: Maybe Addr
    , _gas       :: W256
    , _value     :: W256
    , _nonce     :: W256
    , _input     :: ByteString
    }
    deriving (Show)


parseTx :: (AsValue s, Show s) => s -> Maybe Tx
parseTx receipt = do
  blockNum <- readText <$> receipt ^? key "blockNumber" . _String
  from <- readText <$> receipt ^? key "from" . _String
  gas <- readText <$> receipt ^? key "gas" . _String
  value <- readText <$> receipt ^? key "value" . _String
  nonce <- readText <$> receipt ^? key "nonce" . _String
  input <- encodeUtf8 <$> receipt ^? key "input" . _String
  --
  let to = readText <$> receipt ^? key "to" . _String
  return $ Tx blockNum 0 from to gas value nonce input

parseTxHashesFromBlock :: (AsValue s, Show s) => s -> [Text]
parseTxHashesFromBlock receipt =
  receipt ^.. key "transactions" . values . _String


fetchTx :: Text -> Text -> IO (Maybe Tx)
fetchTx url txhash = do
  sess <- Session.newAPISession
  tx <-
    fetchWithSession url sess (rpc "eth_getTransactionByHash" [String txhash])
  case tx >>= parseTx of
    Just tx' -> do
      block <- fetchBlockFrom (BlockNumber $ _blockNum tx') url
      case block of
        Just block' -> return $ Just $ tx' {_timestamp = EVM._timestamp block'}
        Nothing -> return Nothing
    Nothing -> return Nothing


fetchPriorTxsInSameBlock :: Text -> Text -> IO [Text]
fetchPriorTxsInSameBlock url txHash = do
  tx <- fetchTx url txHash
  case tx of
    Nothing -> return mempty
    Just tx' -> do
      txHashes <- fetchTxHashesInBlock url (_blockNum tx')
      return $ takeWhile ((/=) txHash) txHashes


fetchTxHashesInBlock :: Text -> W256 -> IO [Text]
fetchTxHashesInBlock url bn = do
  sess <- Session.newAPISession
  blockData <- fetchWithSession url sess (rpc "eth_getBlockByNumber" [toRPC bn, toRPC False])
  return $ case blockData of
    Just x  -> parseTxHashesFromBlock x
    Nothing -> mempty
