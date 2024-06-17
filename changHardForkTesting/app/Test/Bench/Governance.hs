{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bench.Governance where

import Cardano.Api qualified as C

import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.KeyMap (mapMaybe)
import Data.Maybe (catMaybes)
import Debug.Trace qualified as Debug
import Hedgehog hiding (test)
import Hedgehog qualified as H
import Helpers.Committee
import Helpers.Common (makeAddress, toConwayEraOnwards, toShelleyBasedEra)
import Helpers.DRep
import Helpers.DRep qualified as DRep
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking
import Helpers.TestData (TestInfo (..), TestParams (..), verifyTxConfirmation)
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.Utils (getChunk, paymentKeyToAddress, pickRandomElements)
import Test.Bench.Users
import Test.Bench.Users qualified as ShelleyWallet

-- register all shelley wallets
-- register all DReps
-- resiter CC Members
-- register all StakePools

--

registerShelleyWalletsTestInfo sw =
    TestInfo
        { testName = "registerShelleyWallets"
        , testDescription =
            "Verify multiple stake address registration for voting in a single transaction."
        , test = registerShelleyWallets sw
        }

registerShelleyWallets ::
    (MonadIO m, MonadTest m) =>
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
registerShelleyWallets
    shelleyWallets
    networkOptions
    testParams = do
        Debug.traceM ("Registering " ++ (show (length shelleyWallets)) ++ " shelley wallets in 5 batches.")
        mapM_ (\x -> registerSelectedShelleyWallets shelleyWallets networkOptions x testParams) [0 .. 4]
        Debug.traceM ((show (length shelleyWallets)) ++ " shelley wallets registered")
        return Nothing

registerSelectedShelleyWallets ::
    (MonadIO m, MonadTest m) =>
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    Int ->
    TestParams era ->
    m (Maybe String)
registerSelectedShelleyWallets
    shelleyWallets
    networkOptions
    chunkIdx
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let ceo = toConwayEraOnwards era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        stakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _) -> skey) shelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            stakeRegistrationRequirements = map (\x -> C.StakeAddrRegistrationConway ceo (C.Lovelace 0) x) stakeCredentials
            stakeRegCerts = map C.makeStakeAddressRegistrationCertificate stakeRegistrationRequirements
            stakeRegCertsChunk = getChunk 100 chunkIdx stakeRegCerts
            stakeCredentialChunk = getChunk 100 chunkIdx stakeCredentials
            stakeSKeysChunk = map C.WitnessStakeKey $ getChunk 100 chunkIdx stakeSKeys
            registerStake =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            stakeRegCertsChunk
                            stakeCredentialChunk
                    , C.txOuts = [stakeDelegTxOut]
                    }
            totalWitnesss = fromIntegral (length stakeSKeysChunk + 1)
        signedStakeReg <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                registerStake
                w1Address
                (Just totalWitnesss)
                ([C.WitnessPaymentKey w1SKey] ++ stakeSKeysChunk)
        Tx.submitTx sbe localNodeConnectInfo signedStakeReg
        let expTxIn = Tx.txIn (Tx.txId signedStakeReg) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        Debug.traceM ("Batch No." ++ (show (chunkIdx + 1)) ++ " containing 100 shelley wallets registered")
        return Nothing

fundShelleyWalletsTestInfo sw =
    TestInfo
        { testName = "fundShelleyWallets"
        , testDescription =
            "Fund multiple shelley wallets in a single transaction."
        , test = fundShelleyWallets sw
        }

fundShelleyWallets ::
    (MonadIO m, MonadTest m) =>
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
fundShelleyWallets
    shelleyWallets
    networkOptions
    testParams = do
        Debug.traceM ("Funding " ++ (show (length shelleyWallets)) ++ " shelley wallets in 10 batches.")
        mapM_ (\x -> fundSelectedShelleyWallets shelleyWallets networkOptions x testParams) [0 .. 9]
        Debug.traceM ((show (length shelleyWallets)) ++ " shelley wallets funded")
        return Nothing

fundSelectedShelleyWallets ::
    (MonadIO m, MonadTest m) =>
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    Int ->
    TestParams era ->
    m (Maybe String)
fundSelectedShelleyWallets
    shelleyWallets
    networkOptions
    chunkIdx
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        fundWalletTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let fundValue = 100_000_000
            shelleyWalletAddresses = map (\(ShelleyWallet _ pkey) -> paymentKeyToAddress pkey networkId) shelleyWallets
            shelleyWalletChunk = getChunk 50 chunkIdx shelleyWalletAddresses
            txOutToShelleyWallet = map (\x -> Tx.txOut era (C.lovelaceToValue fundValue) x) shelleyWalletChunk
        let fundShelleyWallet =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [fundWalletTxIn]
                    , C.txOuts = txOutToShelleyWallet
                    }
        signedFundShelleyWallet <-
            Tx.buildTx
                era
                localNodeConnectInfo
                fundShelleyWallet
                w1Address
                [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedFundShelleyWallet
        let expTxIns = map (\x -> Tx.txIn (Tx.txId signedFundShelleyWallet) x) [0 .. 49]
        mapM_ (\(ti, add) -> verifyTxConfirmation era localNodeConnectInfo add ti) (zip expTxIns shelleyWalletChunk)
        Debug.traceM ("Batch No." ++ (show (chunkIdx + 1)) ++ " containing " ++ (show (length txOutToShelleyWallet)) ++ " shelley wallets funded with " ++ (show fundValue))
        return Nothing

registerDrepsInfo dReps =
    TestInfo
        { testName = "registerDReps"
        , testDescription = "Register DReps"
        , test = do
            let (keyDRepsSkey, keyDrepsCred) = unzip [(skey, cert) | KeyDRep skey _ _ cert _ _ <- dReps]
            registerDReps keyDRepsSkey keyDrepsCred
        }

registerDReps ::
    (MonadTest m, MonadIO m) =>
    [C.SigningKey C.DRepKey] ->
    [C.Certificate era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
registerDReps
    dRepKeys
    dRepRegCert
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        dRepRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            regDRepTxOut = Tx.txOut era (C.lovelaceToValue 1_000_000) w1Address
            regDRepTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [dRepRegTxIn]
                    , C.txCertificates = Tx.txCertificates era dRepRegCert []
                    , C.txOuts = [regDRepTxOut]
                    }
        let
            dRepCount = fromIntegral $ (length dRepKeys + 1)
            dRepWitnesses = map C.WitnessDRepKey dRepKeys
        signedRegDRepTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                regDRepTxBodyContent
                w1Address
                (Just dRepCount)
                ([C.WitnessPaymentKey w1SKey] ++ dRepWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedRegDRepTx
        let expTxIn = Tx.txIn (Tx.txId signedRegDRepTx) 0
        regDRepResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show regDRepResultTxOut
        Debug.traceM ((show (length dRepKeys)) ++ " DReps registered")
        return Nothing

registerCCMembersInfo ccMembers =
    TestInfo
        { testName = "registerCCMembers"
        , testDescription = "Register CC Members"
        , test = registerCCMembers ccMembers
        }

registerCCMembers ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
registerCCMembers
    committee
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        regCCTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            regCCTxOut = Tx.txOut era (C.lovelaceToValue 1_000_000) w1Address
            committeeHotKeyAuthCerts = map (\(Committee _ _ _ _ cert _) -> cert) committee
            committeeColdSkeys = map (\(Committee cold _ _ _ _ _) -> C.WitnessCommitteeColdKey cold) committee
            committeeRegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [regCCTxIn]
                    , C.txCertificates = Tx.txCertificates era committeeHotKeyAuthCerts []
                    , C.txOuts = [regCCTxOut]
                    }
            witnessCount = fromIntegral (length committeeColdSkeys + 1)
        signedCommitteeRegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                committeeRegTxBodyContent
                w1Address
                (Just witnessCount)
                ([C.WitnessPaymentKey w1SKey] ++ committeeColdSkeys)
        Tx.submitTx sbe localNodeConnectInfo signedCommitteeRegTx
        let expTxIn = Tx.txIn (Tx.txId signedCommitteeRegTx) 0
        regDRepResultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show regDRepResultTxOut
        Debug.traceM ((show (length committee)) ++ " Constitutional Committee members registered")
        return Nothing

delegateAdaHolderToDRepsTestInfo adaHolders dreps =
    TestInfo
        { testName = "delegateAdaHolderToDReps"
        , testDescription = "Delegate random shelly wallet (ada holders) to random DReps"
        , test = delegateAdaHolderToDRepsTest adaHolders dreps
        }

delegateAdaHolderToDRepsTest ::
    (MonadTest m, MonadIO m) =>
    [ShelleyWallet era] ->
    [DRep era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
delegateAdaHolderToDRepsTest
    adaHolders
    dReps
    networkOptions
    testParams = do
        mapM_ (\(drep, drepIdx) -> mapM_ (\idx -> delegateAdaHolderToSelectedDReps adaHolders drep drepIdx networkOptions idx testParams) [0 .. 9]) (zip dReps [0 .. 19])
        Debug.traceM ("All " ++ (show (length (adaHolders))) ++ " Ada holders delegated to all " ++ (show (length dReps)) ++ " DReps")
        return Nothing

delegateAdaHolderToSelectedDReps ::
    (MonadTest m, MonadIO m) =>
    [ShelleyWallet era] ->
    DRep era ->
    Int ->
    TN.TestEnvironmentOptions era ->
    Int ->
    TestParams era ->
    m (Maybe String)
delegateAdaHolderToSelectedDReps
    adaHolders
    dRep
    drepIdx
    networkOptions
    chunkIdx
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            adaHolderChunk = getChunk 50 chunkIdx adaHolders
            sKeys = map (ShelleyWallet.stakeSKey) adaHolderChunk
            stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) sKeys
        stakeDelegTxIn1 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            voteDelgCerts = map (\x -> DRep.voteDelegateCert (toConwayEraOnwards era) (kDRepLedgerCred dRep) x) stakeCreds
            stakeDelegTxBodyContent1 =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeDelegTxIn1]
                    , C.txCertificates = Tx.txCertificates era voteDelgCerts stakeCreds
                    , C.txOuts = [stakeDelegTxOut]
                    }
            totalWitness = fromIntegral (length stakeCreds + 1)
            randomWitnesses = map C.WitnessStakeKey sKeys
        signedStakeDelegTx1 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent1
                w1Address
                (Just totalWitness)
                ([C.WitnessPaymentKey w1SKey] ++ randomWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx1
        let expTxIn1 = Tx.txIn (Tx.txId signedStakeDelegTx1) 0
        stakeDelegResultTxOut1 <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn1 "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut1
        Debug.traceM ("Drep " ++ (show (drepIdx + 1)) ++ ": Delegating Batch No." ++ (show (chunkIdx + 1)) ++ " containing " ++ (show (length adaHolderChunk)) ++ " ada holders")
        return Nothing

-- delegate to pools
delegateAdaHolderToStakePoolsTestInfo shelleyWallet stakePools =
    TestInfo
        { testName = "delegateAdaHolderToStakePool"
        , testDescription = "Delegate ada holders to stake pools"
        , test = delegateAdaHolderToStakePool shelleyWallet stakePools
        }

delegateAdaHolderToStakePool ::
    (MonadTest m, MonadIO m) =>
    [ShelleyWallet era] ->
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
delegateAdaHolderToStakePool
    adaHolders
    stakePools
    networkOptions
    testParams = do
        mapM_ (\(stakePool, spIdx) -> (mapM_ (\idx -> delegateSelectedAdaHolderToStakePool adaHolders stakePool spIdx networkOptions idx testParams) [0 .. 9])) (zip stakePools [0 .. 2])
        Debug.traceM ("All " ++ (show (length adaHolders)) ++ " Ada Holders delegated to all " ++ (show (length stakePools)) ++ " Stake Pools")
        return Nothing

delegateSelectedAdaHolderToStakePool ::
    (MonadTest m, MonadIO m) =>
    [ShelleyWallet era] ->
    StakePool era ->
    Int ->
    TN.TestEnvironmentOptions era ->
    Int ->
    TestParams era ->
    m (Maybe a)
delegateSelectedAdaHolderToStakePool
    adaHolders
    stakePool
    spIdx
    networkOptions
    chunkIdx
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        delegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let adaHolderChunk = getChunk 50 chunkIdx adaHolders
            stakeSKeys = map (\(ShelleyWallet skey _) -> skey) adaHolderChunk
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeDelegCerts = map (\x -> stakeDelegCert ceo (sPLedgerKeyHash stakePool) x) stakeCredentials
            stakeDelegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [delegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            stakeDelegCerts
                            stakeCredentials
                    , C.txOuts = [stakeDelegTxOut]
                    }
            stakeWitnesses = map C.WitnessStakeKey stakeSKeys
            totalWitnesses = fromIntegral (length stakeCredentials + 1)
        signedStakeDelegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent
                w1Address
                (Just totalWitnesses)
                ([C.WitnessPaymentKey w1SKey] ++ stakeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        Debug.traceM
            ( "Stake Pool "
                ++ (show (spIdx + 1))
                ++ ": Delegating Batch No: "
                ++ (show (chunkIdx + 1))
                ++ " containing "
                ++ (show (length adaHolderChunk))
                ++ " ada holders"
            )
        return Nothing
