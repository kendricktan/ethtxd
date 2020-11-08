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
import           Data.Monoid                      (mempty)
import           Data.SBV                         (literal)
import           Data.Text                        (Text, pack, unpack)
import           Data.Text.IO                     (hPutStr)
import           Data.Tree                        (Tree (..), levels)
import           System.IO                        (stderr)

import qualified EVM
import qualified EVM.FeeSchedule                  as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Stepper
import qualified EVM.VMTest                       as VMTest

-- txhash 0x44c6f3f304a1e566c14063066f440d82eb7adbae57a3b9bcb1aec7c7dab65766
emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty (SourceCache mempty mempty mempty mempty)

caller :: Addr
caller = Addr 0xaf6ac9e4b68fb3da8fb95fac1e072a2917b4f78f

address :: Addr
address = Addr 0x7a250d5630b4cf539739df2c5dacb4c659f2488d

ethrunAddress :: Addr
ethrunAddress = Addr 0x00a329c0648769a73afac7f9381e08fb43dbea72

url = "http://localhost:8545"

blockNum = 11106093

block = EVM.Fetch.BlockNumber blockNum

fetcher = EVM.Fetch.http block url

contract = EVM.Fetch.fetchContractFrom block url address

calldata =
  ConcreteBuffer $
  decipher
    "0x38ed173900000000000000000000000000000000000000000000006c3f9032c962aeef5900000000000000000000000000000000000000000000002c1f7f98d3e8ba2df200000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000af6ac9e4b68fb3da8fb95fac1e072a2917b4f78f000000000000000000000000000000000000000000000000000000005f9185350000000000000000000000000000000000000000000000000000000000000003000000000000000000000000d533a949740bb3306d119cc777fa900ba034cd52000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc20000000000000000000000006b175474e89094c44da98b954eedeac495271d0f"

codeType = EVM.RuntimeCode

decipher = hexByteString "bytes" . strip0x

value = 0

miner = 0

timestamp = 1604743

diff = 0

vm0 miner ts blockNum diff c =
  EVM.makeVm $
  EVM.VMOpts
    { EVM.vmoptContract = c
    , EVM.vmoptCalldata = (calldata, literal . num $ len calldata)
    , EVM.vmoptValue = w256lit value
    , EVM.vmoptAddress = address
    , EVM.vmoptCaller = litAddr caller
    , EVM.vmoptOrigin = 0
    , EVM.vmoptGas = 0xffffffff
    , EVM.vmoptGaslimit = 0xffffffff
    , EVM.vmoptCoinbase = miner
    , EVM.vmoptNumber = blockNum
    , EVM.vmoptTimestamp = w256lit ts
    , EVM.vmoptBlockGaslimit = 0
    , EVM.vmoptGasprice = 0
    , EVM.vmoptMaxCodeSize = 0xffffffff
    , EVM.vmoptDifficulty = diff
    , EVM.vmoptSchedule = FeeSchedule.istanbul
    , EVM.vmoptChainId = 1
    , EVM.vmoptCreate = False
    , EVM.vmoptStorageModel = EVM.ConcreteS
    }

vm = do
  contract' <-
    contract >>= \case
      Nothing -> error $ "No contract found at " <> show address
      Just c -> return c
  return $ VMTest.initTx $ vm0 miner timestamp blockNum diff contract'

runVM =
  execStateT (EVM.Stepper.interpret fetcher . void $ EVM.Stepper.execFully)

decodeTreeTrace :: Tree EVM.Trace -> [String]
decodeTreeTrace (Node n ns) = cur : concatMap decodeTreeTrace ns
  where
    cur =
      case view EVM.traceData n of
        EventTrace (Log _ _ _) -> "event"
        FrameTrace (CallContext target context _ _ hash abi calldata _ _) ->
          "call " <> show target <> " " <> (unpack $ formatSBinary calldata)
        _ -> "unknown"

main :: IO ()
main = do
  vm' <- vm >>= runVM
  let traces = decodeTreeTrace <$> EVM.traceForest vm'
  print traces
  -- hPutStr stderr (showTraceTree emptyDapp vm')
  -- case view EVM.result vm' of
  --   Nothing -> error "internal error; no EVM result"
  --   Just (EVM.VMFailure (EVM.Revert msg)) -> do
  --     print $ ByteStringS msg
  --     print "no good"
  --   Just (EVM.VMFailure err) -> do
  --     print err
  --     print "no good"
  --   Just (EVM.VMSuccess buf) -> do
  --     let msg =
  --           case buf of
  --             SymbolicBuffer msg' -> forceLitBytes msg'
  --             ConcreteBuffer msg' -> msg'
  --     print $ ByteStringS msg
  --     print "we good"
  return ()
