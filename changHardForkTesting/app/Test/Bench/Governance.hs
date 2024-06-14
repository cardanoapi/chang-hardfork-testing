{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bench.Governance where

import Cardano.Api qualified as C

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.KeyMap (mapMaybe)
import Data.Maybe (catMaybes)
import Hedgehog hiding (test)
import Hedgehog qualified as H
import Helpers.Committee
import Helpers.Common (toConwayEraOnwards, toShelleyBasedEra)
import Helpers.DRep
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Test.Bench.Users

-- register all shelley wallets
-- register all DReps
-- resiter CC Members
-- register all StakePools

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
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let ceo = toConwayEraOnwards era
            (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
        stakeRegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let
            stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            stakeCredentials = map (\(ShelleyWallet _ cred) -> cred) shelleyWallets
            stakeSKeys = map (\(ShelleyWallet skey _) -> skey) shelleyWallets
            stakeRegistrationRequirements = map (\x -> C.StakeAddrRegistrationConway ceo (C.Lovelace 0) x) stakeCredentials
            stakeRegCerts = map C.makeStakeAddressRegistrationCertificate stakeRegistrationRequirements
            witnessStakeSKeys = map C.WitnessStakeKey stakeSKeys
            registerFirst25stake =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [stakeRegTxIn]
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            stakeRegCerts
                            stakeCredentials
                    , C.txOuts = [stakeDelegTxOut]
                    }
        signedStakeRegTx0to25 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                registerFirst25stake
                w1Address
                (Just 101)
                ([C.WitnessPaymentKey w1SKey] ++ witnessStakeSKeys)
        Tx.submitTx sbe localNodeConnectInfo signedStakeRegTx0to25
        let expTxIn0to25 = Tx.txIn (Tx.txId signedStakeRegTx0to25) 0
        stakeDelegResultTxOut0to25 <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn0to25 "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut0to25
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
        return Nothing
