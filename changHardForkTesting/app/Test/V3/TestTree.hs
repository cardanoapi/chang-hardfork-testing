{-# LANGUAGE RecordWildCards #-}

module Test.V3.TestTree where

import Control.Exception.Base
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import Data.Time.Clock.POSIX qualified as Time
import GHC.Base
import GHC.IO.Exception
import Hedgehog qualified as H
import Helpers.Test
import Helpers.TestData
import Helpers.TestResults
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import System.Directory
import System.Exit
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog (testProperty)
import Test.V3.Tests
import Text.XML.Light.Output

data ResultsRefs = ResultsRefs
    { pv6ResultsRef :: IORef [TestResult]
    , pv7ResultsRef :: IORef [TestResult]
    , pv8ResultsRef :: IORef [TestResult]
    , pv9ResultsRef :: IORef [TestResult]
    , pv9GovResultsRef :: IORef [TestResult]
    }

pv9Tests :: IORef [TestResult] -> H.Property
pv9Tests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    preTestnetTime <- liftIO Time.getCurrentTime
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams

    -- checkTxInfo tests must be first to run after new testnet is initialised due to expected slot to posix time
    sequence_
        [ run verifyBLS12G2ForUtxoUnlockingTestInfo
        , run verifyBLS12G1ForUtxoUnlockingTestInfo
        , run verifySchnorrSignatureForUtxoUnlockingTestInfo
        , run verifyKeccak256ForUtxoUnlockingTestInfo
        ]
    failureMessages <- liftIO $ suiteFailureMessages resultsRef
    liftIO $ putStrLn $ "\nNumber of test failures in suite: " ++ (show $ length failureMessages)
    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

tests :: ResultsRefs -> TestTree
tests ResultsRefs{..} =
    testGroup
        "Plutus E2E Tests"
        [ -- Alonzo PV6 environment has "Chain not extended" error on start
          -- testProperty "Alonzo PV6 Tests" (pv6Tests pv6ResultsRef)
          --   testProperty "Babbage PV7 Tests" (pv7Tests pv7ResultsRef)
          -- , testProperty "Babbage PV8 Tests" (pv8Tests pv8ResultsRef)
          testProperty "Conway PV9 Tests" (pv9Tests pv9ResultsRef)
          -- , testProperty "Conway PV9 Governance Tests" (pv9GovernanceTests pv9GovResultsRef)
          -- testProperty "Write Serialised Script Files" writeSerialisedScriptFiles
          --  testProperty "debug" (debugTests pv8ResultsRef)
          -- testProperty "Babbage PV8 Tests (on Preview testnet)" (localNodeTests pv8ResultsRef TN.localNodeOptionsPreview)
        ]

runTestsWithResults :: IO ()
runTestsWithResults = do
    createDirectoryIfMissing False "test-report-xml"
    allRefs@[pv6ResultsRef, pv7ResultsRef, pv8ResultsRef, pv9ResultsRef, pv9GovResultsRef] <-
        traverse newIORef $ replicate 5 []
    eException <-
        try
            ( defaultMain $
                tests $
                    ResultsRefs pv6ResultsRef pv7ResultsRef pv8ResultsRef pv9ResultsRef pv9GovResultsRef
            ) ::
            IO (Either ExitCode ())
    [pv6Results, pv7Results, pv8Results, pv9Results, pv9GovResults] <- traverse readIORef allRefs
    failureMessages <- liftIO $ allFailureMessages allRefs
    liftIO $ putStrLn $ "Total number of test failures: " ++ (show $ length failureMessages)

    let pv6TestSuiteResult = TestSuiteResults "Alonzo PV6 Tests" pv6Results
        pv7TestSuiteResult = TestSuiteResults "Babbage PV7 Tests" pv7Results
        pv8TestSuiteResult = TestSuiteResults "Babbage PV8 Tests" pv8Results
        pv9TestSuiteResult = TestSuiteResults "Conway PV9 Tests" pv9Results
        pv9GovernanceTestSuiteResult = TestSuiteResults "Conway PV9 Governanace Tests" pv9GovResults

    -- Use 'results' to generate custom JUnit XML report
    let xml =
            testSuitesToJUnit
                [ pv6TestSuiteResult
                , pv7TestSuiteResult
                , pv8TestSuiteResult
                , pv9TestSuiteResult
                , pv9GovernanceTestSuiteResult
                ]
    writeFile "test-report-xml/test-results.xml" $ showTopElement xml

    when (eException /= Left ExitSuccess || length failureMessages > 0) exitFailure
