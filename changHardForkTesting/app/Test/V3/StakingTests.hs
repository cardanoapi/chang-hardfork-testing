{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.StakingTests where

import Cardano.Api qualified as C

import Control.Monad.IO.Class (MonadIO (liftIO))
import Debug.Trace qualified as Debug
import Hedgehog hiding (test)
import Hedgehog qualified as H
import Helpers.Common (toConwayEraOnwards, toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils (addEpoch)
import Utils (consoleLog)

-- mutiple stake address registration
verifyMultipleStakeAddressRegistrationTestInfo :: [Staking era] -> TestInfo era
verifyMultipleStakeAddressRegistrationTestInfo staking =
    TestInfo
        { testName = "verifyMultipleStakeAddressRegistrationTest"
        , testDescription =
            "Verify multiple stake address registration in a single transaction."
        , test = verifyMultipleStakeAddressRegistrationTest staking
        }

verifyMultipleStakeAddressRegistrationTest ::
    (MonadIO m, MonadTest m) =>
    [Staking era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultipleStakeAddressRegistrationTest
    staking
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        stakeDelegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            staking1 = staking !! 0
            staking2 = staking !! 1
            staking3 = staking !! 2
            stakeDelegTxOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            stakeDelegTxOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeDelegTxOut3 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            stakeRegCert1 = stakeRegCert staking1
            stakeRegCert2 = stakeRegCert staking2
            stakeRegCert3 = stakeRegCert staking3
            stakeDelegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeDelegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [stakeRegCert1, stakeRegCert2, stakeRegCert3]
                            [(stakeCred staking1), (stakeCred staking2), (stakeCred staking3)]
                    , C.txOuts = [stakeDelegTxOut1, stakeDelegTxOut2, stakeDelegTxOut3]
                    }
        signedStakeDelegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent
                w1Address
                (Just 4)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakeKey (stakeSKey staking1)
                , C.WitnessStakeKey (stakeSKey staking2)
                , C.WitnessStakeKey (stakeSKey staking3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        consoleLog ((show (length staking)) ++ " Stake Addresses registered")
        return Nothing

-- register multiple stake pool
verifyMultipleStakePoolRegistrationTestInfo :: [StakePool era] -> TestInfo era
verifyMultipleStakePoolRegistrationTestInfo stakePool =
    TestInfo
        { testName = "verifyMultipleStakePoolRegistrationTest"
        , testDescription = "Register multiple stake pools in a single transaction"
        , test = verifyMultipleStakePoolRegistrationTest stakePool
        }

verifyMultipleStakePoolRegistrationTest ::
    (MonadTest m, MonadIO m) =>
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultipleStakePoolRegistrationTest
    stakePool
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let sbe = toShelleyBasedEra era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
        sPRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            spRegCerts = map (\sp -> sPRegCert sp) stakePool
            spStakeCreds = map (\sp -> sPStakeCred sp) stakePool
            regSPTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            regSPTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [sPRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            spRegCerts
                            spStakeCreds
                    , C.txOuts = [regSPTxOut]
                    }
            spWitnessesPoolKey = map (\sp -> C.WitnessStakePoolKey (sPSKey sp)) stakePool
            spWitnessStakeKey = map (\sp -> C.WitnessStakeKey (sPRewardKey sp)) stakePool
            totalWitnesses = fromIntegral (length (spWitnessesPoolKey ++ spWitnessStakeKey) + 1)
        signedRegSPTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                regSPTxBodyContent
                w1Address
                (Just totalWitnesses)
                ([C.WitnessPaymentKey w1SKey] ++ spWitnessStakeKey ++ spWitnessesPoolKey)
        Tx.submitTx sbe localNodeConnectInfo signedRegSPTx
        let expTxIn = Tx.txIn (Tx.txId signedRegSPTx) 0
        regStakePoolResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show regStakePoolResultTxOut
        consoleLog ((show (length stakePool)) ++ " Stake Pools registered")
        return Nothing

-- delegation to multiple stake pools in a single transaction
verifyMultipleStakePoolDelgationTestInfo :: [Staking era] -> TestInfo era
verifyMultipleStakePoolDelgationTestInfo staking =
    TestInfo
        { testName = "verifyMultipleStakePoolDelgationTest"
        , testDescription = "Delegate to multiple stake pools with different stake keys in a single transaction"
        , test = verifyMultipleStakePoolDelgationTest staking
        }

verifyMultipleStakePoolDelgationTest ::
    (MonadTest m, MonadIO m) =>
    [Staking era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultipleStakePoolDelgationTest
    staking
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let ceo = toConwayEraOnwards era
            sbe = toShelleyBasedEra era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
        stakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        -- register stake addresses
        let staking1 = staking !! 0
            staking2 = staking !! 1
            staking3 = staking !! 2
            stakeDelegTxOut1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            stakeDelegTxOut2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeDelegTxOut3 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            stakeRegCert1 = stakeRegCert staking1
            stakeRegCert2 = stakeRegCert staking2
            stakeRegCert3 = stakeRegCert staking3
            stakeRegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [stakeRegCert1, stakeRegCert2, stakeRegCert3]
                            [(stakeCred staking1), (stakeCred staking2), (stakeCred staking3)]
                    , C.txOuts = [stakeDelegTxOut1, stakeDelegTxOut2, stakeDelegTxOut3]
                    }
        signedStakeRegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeRegTxBodyContent
                w1Address
                (Just 4)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakeKey (stakeSKey staking1)
                , C.WitnessStakeKey (stakeSKey staking2)
                , C.WitnessStakeKey (stakeSKey staking3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedStakeRegTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeRegTx) 0
        stakeRegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeRegResultTxOut
        -- registering stake pools
        spRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            stakePool1 = stakeDelegationPool staking1
            stakePool2 = stakeDelegationPool staking2
            stakePool3 = stakeDelegationPool staking3
            regSPTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            regSPTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [spRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [(sPRegCert stakePool1), (sPRegCert stakePool2), (sPRegCert stakePool3)]
                            [(sPStakeCred stakePool1), (sPStakeCred stakePool2), (sPStakeCred stakePool3)]
                    , C.txOuts = [regSPTxOut]
                    }
        signedRegSPTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                regSPTxBodyContent
                w1Address
                (Just 7)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakePoolKey (sPSKey stakePool1)
                , C.WitnessStakePoolKey (sPSKey stakePool2)
                , C.WitnessStakePoolKey (sPSKey stakePool3)
                , C.WitnessStakeKey (sPRewardKey stakePool1)
                , C.WitnessStakeKey (sPRewardKey stakePool2)
                , C.WitnessStakeKey (sPRewardKey stakePool3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedRegSPTx
        let expTxIn = Tx.txIn (Tx.txId signedRegSPTx) 0
        regStakePoolResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show regStakePoolResultTxOut
        -- delegating to stake pools
        stakeDelegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let stakeDelgCert1 = stakeDelegCert ceo (sPLedgerKeyHash stakePool1) (stakeCred staking1)
            stakeDelgCert2 = stakeDelegCert ceo (sPLedgerKeyHash stakePool2) (stakeCred staking2)
            stakeDelgCert3 = stakeDelegCert ceo (sPLedgerKeyHash stakePool3) (stakeCred staking3)
            stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeDelegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeDelegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [stakeDelgCert1, stakeDelgCert2, stakeDelgCert3]
                            [stakeCred staking1, stakeCred staking2, stakeCred staking3]
                    , C.txOuts = [stakeDelegTxOut]
                    }
        signedStakeDelegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent
                w1Address
                (Just 4)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakeKey (stakeSKey staking1)
                , C.WitnessStakeKey (stakeSKey staking2)
                , C.WitnessStakeKey (stakeSKey staking3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        return Nothing

verifyMultipleStakeAddressDeRegistraionTestInfo :: [Staking era] -> TestInfo era
verifyMultipleStakeAddressDeRegistraionTestInfo staking =
    TestInfo
        { testName = "verifyMultipleStakeAddressDeRegistraionTest"
        , testDescription = "DeRegister multiple stake addresses in a single transaction"
        , test = verifyMultipleStakeAddressDeRegistraionTest staking
        }

verifyMultipleStakeAddressDeRegistraionTest ::
    (MonadTest m, MonadIO m) =>
    [Staking era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultipleStakeAddressDeRegistraionTest
    staking
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let
            sbe = toShelleyBasedEra era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
        stakeDeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            staking1 = staking !! 0
            staking2 = staking !! 1
            staking3 = staking !! 2
            stakeDeRegTxOut = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            stakeDeRegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeDeRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [stakeUnregCert staking1, stakeUnregCert staking2, stakeUnregCert staking3]
                            [stakeCred staking1, stakeCred staking2, stakeCred staking3]
                    , C.txOuts = [stakeDeRegTxOut]
                    }
        signedStakeUnregTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDeRegTxBodyContent
                w1Address
                (Just 4)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakeKey (stakeSKey staking1)
                , C.WitnessStakeKey (stakeSKey staking2)
                , C.WitnessStakeKey (stakeSKey staking3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedStakeUnregTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeUnregTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        return Nothing

verifyMultipleStakePoolRetireTestInfo :: [StakePool era] -> TestInfo era
verifyMultipleStakePoolRetireTestInfo staking =
    TestInfo
        { testName = "verifyMultipleStakePoolRetireTest"
        , testDescription = "Retire multiple stake pools in a single transaction"
        , test = verifyMultipleStakePoolRetireTest staking
        }

verifyMultipleStakePoolRetireTest ::
    (MonadTest m, MonadIO m) =>
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultipleStakePoolRetireTest
    stakePool
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let ceo = toConwayEraOnwards era
            sbe = toShelleyBasedEra era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
        spRetireTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        currentEpoch <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch
        let stakePool1 = stakePool !! 0
            stakePool2 = stakePool !! 1
            stakePool3 = stakePool !! 2
            retireSPCert1 = makeStakePoolRetireCertification ceo stakePool1 (currentEpoch `addEpoch` 1)
            retireSPCert2 = makeStakePoolRetireCertification ceo stakePool2 (currentEpoch `addEpoch` 1)
            retireSPCert3 = makeStakePoolRetireCertification ceo stakePool3 (currentEpoch `addEpoch` 1)
            spRetireTxOut = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            stakeDelegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [spRetireTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            [retireSPCert1, retireSPCert2, retireSPCert3]
                            [(sPStakeCred stakePool1), (sPStakeCred stakePool2), (sPStakeCred stakePool3)]
                    , C.txOuts = [spRetireTxOut]
                    }
        signedPoolRetireTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent
                w1Address
                (Just 4)
                [ C.WitnessPaymentKey w1SKey
                , C.WitnessStakePoolKey (sPSKey stakePool1)
                , C.WitnessStakePoolKey (sPSKey stakePool2)
                , C.WitnessStakePoolKey (sPSKey stakePool3)
                ]
        Tx.submitTx sbe localNodeConnectInfo signedPoolRetireTx
        let expTxIn = Tx.txIn (Tx.txId signedPoolRetireTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        return Nothing
