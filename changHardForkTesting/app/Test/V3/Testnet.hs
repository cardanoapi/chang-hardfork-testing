{-# LANGUAGE RecordWildCards #-}

module Test.V3.Testnet where

import Cardano.Api
import Control.Concurrent (threadDelay)
import Control.Exception.Base
import GHC.IO.Exception
import GHC.IORef
import Hedgehog qualified as H
import Helpers.Test
import Helpers.TestResults
import Helpers.Testnet qualified as TN
import Helpers.Utils qualified as U
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Hedgehog
import Utils

data TestnetRef = TestnetRef
    {testnetResultsRef :: IORef [TestResult]}

testnetTest :: IORef [TestResult] -> H.Property
testnetTest resultsRef = integrationClusterWorkspace 0 "testnet" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9Governance
    cluster <- liftIO $ TN.clusterFilePath
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options cluster
    consoleLog ("\n\nStarted testnet on " ++ cluster)
    consoleLog ("\nNetworkId: " ++ (case networkId of Testnet (NetworkMagic num) -> show num))
    liftIO $ threadDelay (24 * 60 * 60 * 1000000) -- lasts one day
    U.anyLeftFail_ $ TN.cleanupTestnet mPoolNodes
    return ()

toTestTree TestnetRef{..} =
    testGroup
        "TESTNET"
        [ testProperty "Start Testnet" (testnetTest testnetResultsRef)
        ]

runTestnet :: IO ()
runTestnet = do
    testnetRef@[testnet] <- traverse newIORef $ replicate 1 []
    exception <-
        try
            (defaultMain $ toTestTree $ TestnetRef testnet) ::
            IO (Either ExitCode ())
    case exception of
        Left ec -> error ("ExitCode: " ++ (show ec))
        Right _ -> pure ()
