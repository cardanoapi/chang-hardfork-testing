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
import Test.Tasty
import Test.Tasty.Hedgehog
import Utils

data TestnetRef = TestnetRef
    {testnetResultsRef :: IORef [TestResult]}

clusterFilePath :: FilePath
clusterFilePath = "/home/reeshav/chang-hardfork-testing/.cluster"

testnetTest :: IORef [TestResult] -> H.Property
testnetTest resultsRef = integrationClusterWorkspace 0 "testnet" $ \tempAbsPath -> do
    let options = TN.testnetOptionsConway9Governance
    (localNodeConnectInfo, pparams, networkId, mPoolNodes) <-
        TN.setupTestEnvironment options clusterFilePath
    consoleLog ("\n\nStarted testnet on " ++ clusterFilePath)
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
