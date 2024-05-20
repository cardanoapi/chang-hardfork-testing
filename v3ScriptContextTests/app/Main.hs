module Main where
import Test.Tasty
import Test.V3.TestTree (runTestsWithResults)
import GHC.IO.Handle
import GHC.IO.StdHandles
import GHC.IO.Encoding

main :: IO ()
main = do
  hSetEncoding stdout utf8
  runTestsWithResults
  