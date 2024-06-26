module Main where

import GHC.IO.Encoding
import GHC.IO.Handle
import GHC.IO.StdHandles
import System.Environment (lookupEnv)
import Test.V3.TestTree (runTestsWithResults)
import Test.V3.Testnet

main :: IO ()
main = do
    hSetEncoding stdout utf8
    maybeStartTestnet <- lookupEnv "START_TESTNET"
    case maybeStartTestnet of
        Just "1" -> runTestnet
        _ -> runTestsWithResults
