module Main where

import GHC.IO.Encoding
import GHC.IO.Handle
import GHC.IO.StdHandles
import Test.V3.DummyDataTypes
import Test.V3.TestTree (runTestsWithResults)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    runTestsWithResults
