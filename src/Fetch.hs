{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import           Control.Lens         hiding ((.=))
import           Data.Aeson           (Value (..))
import           Data.Aeson.Lens      (AsValue, key, _String)
import           Data.ByteString      (ByteString)
import           Data.Text            (Text (..), pack)
import           Data.Text.Encoding   (encodeUtf8)
import           EVM.Fetch            (BlockNumber (..), fetchBlockFrom,
                                       fetchWithSession, readText, rpc)
import           EVM.Symbolic         (SymWord)
import           EVM.Types            (Addr, W256)

import qualified EVM                  as EVM
import qualified Network.Wreq.Session as Session

data Tx = Tx
    { _blockNum  :: W256
    , _timestamp :: SymWord
    , _from      :: Addr
    , _to        :: Addr
    , _gas       :: W256
    , _value     :: W256
    , _input     :: ByteString
    }
    deriving (Show)

parseTx :: (AsValue s, Show s) => s -> Maybe Tx
parseTx receipt = do
  blockNum <- readText <$> receipt ^? key "blockNumber" . _String
  from <- readText <$> receipt ^? key "from" . _String
  to <- readText <$> receipt ^? key "to" . _String
  gas <- readText <$> receipt ^? key "gas" . _String
  value <- readText <$> receipt ^? key "value" . _String
  input <- encodeUtf8 <$> receipt ^? key "input" . _String
  -- Tx is executed in the state at the prior block
  return $ Tx (blockNum - 1) 0 from to gas value input

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
