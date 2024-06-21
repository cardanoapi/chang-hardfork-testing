{-# LANGUAGE RecordWildCards #-}

module Test.V3.TestTree where

import Control.Exception.Base
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import Data.Time.Clock.POSIX qualified as Time
import GHC.Base
import GHC.IO.Exception
import Hedgehog qualified as H
import Helpers.Committee (generateCommitteeKeysAndCertificate)
import Helpers.Common
import Helpers.DRep (generateDRepKeyCredentialsAndCertificate)
import Helpers.StakePool
import Helpers.Staking
import Helpers.Test
import Helpers.TestData
import Helpers.TestResults
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import System.Directory
import System.Exit
import Test.Bench.Governance
import Test.Bench.Users (generateShelleyWallet)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.V3.EfficiencyTests
import Test.V3.PlutusTests
import Test.V3.StakingTests
import Text.XML.Light.Output
import Prelude hiding (mapM)

data ResultsRefs = ResultsRefs
    { plutusV3ResultsRef :: IORef [TestResult]
    , spendingResultsRef :: IORef [TestResult]
    , referenceInputResultsRef :: IORef [TestResult]
    , mintingResultsRef :: IORef [TestResult]
    , efficiencyResultsRef :: IORef [TestResult]
    , stakingResultsRef :: IORef [TestResult]
    , governanceBenchmarkResultsRef :: IORef [TestResult]
    }

efficiencyTests :: IORef [TestResult] -> H.Property
efficiencyTests resultsRef = integrationRetryWorkspace 0 "efficiency" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    (localNodeConnectInfo, pparams, networkId, _) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [ run verifyV3MintingEfficiencyTestInfo
        , run verifyV3MultiSigEfficiencyTestInfo
        , run verifyV3TxInfoFieldsTestInfo
        ]

stakingTests :: IORef [TestResult] -> H.Property
stakingTests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
        ceo = toConwayEraOnwards $ TN.eraFromOptions options
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    stakePools <- mapM id $ take 3 $ repeat $ generateStakePoolKeyCredentialsAndCertificate ceo networkId
    let [sp1, sp2, sp3] = stakePools
    staking <- generateStakeKeyCredentialAndCertificate ceo sp1
    multiPoolStaking1 <- generateStakeKeyCredentialAndCertificate ceo sp1
    multiPoolStaking2 <- generateStakeKeyCredentialAndCertificate ceo sp2
    multiPoolStaking3 <- generateStakeKeyCredentialAndCertificate ceo sp3
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        multiPoolStaking = [multiPoolStaking1 !! 0, multiPoolStaking2 !! 1, multiPoolStaking3 !! 2]
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [ run $ verifyMultipleStakeAddressRegistrationTestInfo staking
        , run $ verifyMultipleStakePoolRegistrationTestInfo stakePools
        , run $ verifyMultipleStakePoolDelgationTestInfo multiPoolStaking
        , run $ verifyMultipleStakeAddressDeRegistraionTestInfo staking
        , run $ verifyMultipleStakePoolRetireTestInfo stakePools
        ]

plutusV3Tests :: IORef [TestResult] -> H.Property
plutusV3Tests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [ run verifyBLS12G2ForUtxoUnlockingTestInfo
        , run verifyBLS12G1ForUtxoUnlockingTestInfo
        , run verifySchnorrSignatureForUtxoUnlockingTestInfo
        , run verifyKeccak256ForUtxoUnlockingTestInfo
        , run verifyEcdsaSignatureForUtxoUnlockingTestInfo
        , run verifyEd25519SignatureForUtxoUnlockingTestInfo
        , run verifyBlake2b224ForValidatingPubKeyHashTestInfo
        ]
    failureMessages <- liftIO $ suiteFailureMessages resultsRef
    liftIO $ putStrLn $ "\nNumber of test failures in suite: " ++ (show $ length failureMessages)
    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes

spendingTests :: IORef [TestResult] -> H.Property
spendingTests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [ run verifyLockingAndSpendingInSameScriptTestInfo
        , run verifyLockingAndSpendingInDifferentScriptTestInfo
        , run verifyMultiSigRequirementTestInfo
        ]

mintingTests :: IORef [TestResult] -> H.Property
mintingTests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [run verifyMaxExUnitsMintingTestInfo]

referenceInputTests :: IORef [TestResult] -> H.Property
referenceInputTests resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [run verifyReferenceInputVisibilityTestInfo]

pv9GovernanceBenchmark :: IORef [TestResult] -> H.Property
pv9GovernanceBenchmark resultsRef = integrationRetryWorkspace 0 "pv9" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9Governance
        ceo = toConwayEraOnwards $ TN.eraFromOptions options
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options tempAbsPath
    preTestnetTime <- liftIO Time.getCurrentTime
    shelleyWallets <- generateShelleyWallet
    dReps <- generateDRepKeyCredentialsAndCertificate ceo
    ccMembers <- generateCommitteeKeysAndCertificate ceo
    stakePools <- mapM id $ take 3 $ repeat $ generateStakePoolKeyCredentialsAndCertificate ceo networkId
    let testParams = TestParams localNodeConnectInfo pparams networkId tempAbsPath (Just preTestnetTime)
        run testInfo = runTest testInfo resultsRef options testParams
    sequence_
        [ -- commenting this one out for now, because it takes a lot of time
          --   run $ fundShelleyWalletsTestInfo shelleyWallets
          run $ registerShelleyWalletsTestInfo shelleyWallets
        , run $ registerDrepsInfo dReps
        , run $ registerCCMembersInfo ccMembers
        , run $ verifyMultipleStakePoolRegistrationTestInfo stakePools
        , run $ delegateAdaHolderToDRepsTestInfo shelleyWallets dReps
        , run $ delegateAdaHolderToStakePoolsTestInfo shelleyWallets stakePools
        , run $ multipleConstitutionProposalAndVotesTestInfo ccMembers dReps shelleyWallets
        , run $ multipleCommitteeProposalAndVoteTestInfo ccMembers dReps shelleyWallets stakePools
        , run $ multipleNoConfidenceProposalAndVoteTestInfo dReps shelleyWallets stakePools
        , run $ multiplePrameterChangeProposalAndVoteTestInfo ccMembers dReps shelleyWallets
        , run $ multipleTreasuryWithdrawalProposalAndVoteTestInfo ccMembers dReps shelleyWallets
        , run $ multipleInfoProposalAndVoteTestInfo ccMembers dReps shelleyWallets stakePools
        ]

tests :: ResultsRefs -> TestTree
tests ResultsRefs{..} =
    testGroup
        "Plutus E2E Tests"
        [ --     testProperty "Plutus V3 Tests" (plutusV3Tests plutusV3ResultsRef)
          -- , testProperty "Spending V3 Script Tests" (spendingTests spendingResultsRef)
          -- , testProperty "Reference Input Tests" (referenceInputTests referenceInputResultsRef)
          -- , testProperty "Minting Tests" (mintingTests mintingResultsRef)
          -- , testProperty "PlutusV3 Efficiency Tests" (efficiencyTests efficiencyResultsRef)
          -- , testProperty "Staking and Pool Operations Tests" (stakingTests stakingResultsRef)
          testProperty "Governance Actions Benchmark Tests" (pv9GovernanceBenchmark governanceBenchmarkResultsRef)
        ]

runTestsWithResults :: IO ()
runTestsWithResults = do
    createDirectoryIfMissing False "test-report-xml"
    allRefs@[plutusV3ResultsRef, spendingResultsRef, referenceInputResultsRef, mintingResultsRef, efficiencyResultsRef, stakingResultsRef, governanceBenchmarkResultsRef] <-
        traverse newIORef $ replicate 7 []
    eException <-
        try
            ( defaultMain $
                tests $
                    ResultsRefs
                        plutusV3ResultsRef
                        spendingResultsRef
                        referenceInputResultsRef
                        mintingResultsRef
                        efficiencyResultsRef
                        stakingResultsRef
                        governanceBenchmarkResultsRef
            ) ::
            IO (Either ExitCode ())
    [plutusV3Results, spendingResults, referenceInputResults, mintingResults, efficiencyResults, stakingReults, governanceBenchmarkResults] <- traverse readIORef allRefs
    failureMessages <- liftIO $ allFailureMessages allRefs
    liftIO $ putStrLn $ "Total number of test failures: " ++ (show $ length failureMessages)
    let
        plutusV3TestSuiteResults = TestSuiteResults "Plutus V3 Tests" plutusV3Results
        spendingTestSuiteResults = TestSuiteResults "Spending Tx Tests" spendingResults
        referenceInputTestSuiteResults = TestSuiteResults "Reference Input Tx Tests" referenceInputResults
        mintingTestSuiteResults = TestSuiteResults "Minting Tx Tests" mintingResults
        efficiencyTestSuiteResult = TestSuiteResults "PlutusV3 Efficiency Tests" efficiencyResults
        stakeAndPoolTestSuiteResult = TestSuiteResults "Staking and Pool Operations Tests" stakingReults
        governanceBenchmarkResult = TestSuiteResults "Governance Actions Benchmark Tests" governanceBenchmarkResults
    -- Use 'results' to generate custom JUnit XML report
    let xml =
            testSuitesToJUnit
                [ plutusV3TestSuiteResults
                , spendingTestSuiteResults
                , referenceInputTestSuiteResults
                , mintingTestSuiteResults
                , efficiencyTestSuiteResult
                , stakeAndPoolTestSuiteResult
                , governanceBenchmarkResult
                ]
    writeFile "test-report-xml/test-results.xml" $ showTopElement xml

    when (eException /= Left ExitSuccess || length failureMessages > 0) exitFailure
