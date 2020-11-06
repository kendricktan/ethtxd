module Main where

import           EVM.Dapp     (DappInfo, dappInfo)
import           EVM.Solidity (SourceCache (..))

import           Data.Monoid  (mempty)

import qualified EVM
import qualified EVM.Stepper

emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty (SourceCache mempty mempty mempty mempty)

main :: IO ()
main = putStrLn "Hello, Haskell!"
