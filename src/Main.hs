{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           EVM.Dapp                         (DappInfo, dappInfo)
import           EVM.Format                       (showTraceTree)
import           EVM.Solidity                     (SourceCache (..))
import           EVM.Symbolic                     (len, litAddr, w256lit)
import           EVM.Types

import           Control.Monad                    (void)
import           Control.Monad.Trans.State.Strict
import           Data.ByteString                  (ByteString)

import           Data.Monoid                      (mempty)
import           Data.SBV                         (literal)
import           Data.Text.IO                     (hPutStr)
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
caller = read "0x7ee39071c8e207bea9ed01d27d195f3a6cc886ac"

address :: Addr
address = read "0x7a250d5630b4cf539739df2c5dacb4c659f2488d"

url = "http://localhost:8545"

blockNum = 11209179

block = EVM.Fetch.BlockNumber blockNum

fetcher = EVM.Fetch.http block url

contract = EVM.Fetch.fetchContractFrom block url address

calldata :: ByteString
calldata =
  "0x18cbafe500000000000000000000000000000000000000000000000000000000283828e300000000000000000000000000000000000000000000000014749f446f3e3b6700000000000000000000000000000000000000000000000000000000000000a00000000000000000000000007ee39071c8e207bea9ed01d27d195f3a6cc886ac000000000000000000000000000000000000000000000000000000005fa65c8e0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000dac17f958d2ee523a2206206994597c13d831ec7000000000000000000000000c02aaa39b223fe8d0a0e5c4f27ead9083c756cc2"

codeType = EVM.RuntimeCode

decipher = hexByteString "bytes" . strip0x

block' = block

value' = 0

caller' = caller

origin' = caller -- Maybe change

calldata' = ConcreteBuffer calldata

address' = address

miner = 0

timestamp = 1604743

diff = 0

vm0 miner ts blockNum diff c =
  EVM.makeVm $
  EVM.VMOpts
    { EVM.vmoptContract = c
    , EVM.vmoptCalldata = (calldata', literal . num $ len calldata')
    , EVM.vmoptValue = w256lit value'
    , EVM.vmoptAddress = address'
    , EVM.vmoptCaller = litAddr caller'
    , EVM.vmoptOrigin = origin'
    , EVM.vmoptGas = 0xffffffffffffffff
    , EVM.vmoptGaslimit = 0xffffffffffffffff
    , EVM.vmoptCoinbase = miner
    , EVM.vmoptNumber = blockNum
    , EVM.vmoptTimestamp = ts
    , EVM.vmoptBlockGaslimit = 1000000000000
    , EVM.vmoptGasprice = 1
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

main :: IO ()
main = do
  vm' <- vm >>= runVM
  hPutStr stderr (showTraceTree emptyDapp vm')
  return ()
