{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bench.Governance where

import Cardano.Api qualified as C

import Cardano.Api (queryGovState)
import Cardano.Api.Ledger (strictMaybeToMaybe)
import Cardano.Api.Ledger qualified as CL
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Crypto.Hash.Blake2b qualified as Crypto
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Conway.Governance qualified (ConwayEraGov (proposalsGovStateL))
import Cardano.Ledger.Conway.Governance qualified as CG
import Cardano.Ledger.Conway.PParams qualified as L
import Cardano.Ledger.Core qualified as L
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson.KeyMap (mapMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short (toShort)
import Data.ByteString.Short qualified as LBS
import Data.Function
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as Text
import Debug.Trace qualified as Debug
import GHC.Conc.IO (threadDelay)
import GHC.Num
import GHC.Real ((%))
import Hedgehog hiding (test)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Helpers.Committee
import Helpers.Committee qualified as CC
import Helpers.Common (makeAddress, toConwayEraOnwards, toShelleyBasedEra)
import Helpers.DRep
import Helpers.DRep qualified as DRep
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking
import Helpers.Test qualified
import Helpers.TestData (TestInfo (..), TestParams (..), verifyTxConfirmation)
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters (toPlutusAddress)
import Helpers.Utils (GovPurpose (..), addEpoch, getChunk, getPrevGovAction, paymentKeyToAddress, pickRandomElements)
import Test.Bench.Users
import Test.Bench.Users qualified as ShelleyWallet
import Utils (consoleLog)

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
        consoleLog ("Registering " ++ (show (length shelleyWallets)) ++ " shelley wallets in 5 batches.")
        mapM_ (\x -> registerSelectedShelleyWallets shelleyWallets networkOptions x testParams) [0, 1]
        consoleLog ((show (length shelleyWallets)) ++ " shelley wallets registered")
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
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) shelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            stakeRegistrationRequirements = map (\x -> C.StakeAddrRegistrationConway ceo (L.Coin 0) x) stakeCredentials
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
        consoleLog ("Batch No." ++ (show (chunkIdx + 1)) ++ " containing " ++ (show $ length stakeSKeysChunk) ++ " shelley wallets registered")
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
        consoleLog ("Funding " ++ (show (length shelleyWallets)) ++ " shelley wallets in 10 batches.")
        let stakeKeys = map ShelleyWallet.stakeSKey shelleyWallets
            paymentKeys = map ShelleyWallet.paymentSKey shelleyWallets
            stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) (stakeKeys)
            shelleyWalletAddresses = map (\(ShelleyWallet _ _ addr) -> addr) shelleyWallets
            shelleyPlutusAddresses = map (\s -> C.serialiseAddress $ C.anyAddressInShelleyBasedEra C.ShelleyBasedEraConway (C.toAddressAny s)) shelleyWalletAddresses
            stakeAddress = map (\x -> C.serialiseAddress $ C.makeStakeAddress (C.Testnet $ C.NetworkMagic 42) x) stakeCreds
        liftIO $
            LBS.writeFile
                ".shelleyWallets/walletAddresses.json"
                (LBS.fromStrict $ BS8.pack $ "{\"shelleyWallets\": " ++ show shelleyPlutusAddresses ++ "}")
        liftIO $
            LBS.writeFile
                ".shelleyWallets/stakeAddresses.json"
                (LBS.fromStrict $ BS8.pack $ "{\"stakeAddresses\": " ++ show stakeAddress ++ "}")
        liftIO $
            LBS.writeFile
                ".shelleyWallets/paymentKeys.json"
                (LBS.fromStrict $ BS8.pack $ "{\"paymentKeys\": " ++ show paymentKeys ++ "}")
        liftIO $
            LBS.writeFile
                ".shelleyWallets/stakeKeys.json"
                (LBS.fromStrict $ BS8.pack $ "{\"stakeKeys\": " ++ show stakeKeys ++ "}")
        mapM_ (\x -> fundSelectedShelleyWallets shelleyWallets networkOptions x testParams) [0 .. 2]
        consoleLog ((show (length shelleyWallets)) ++ " shelley wallets funded")
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
            shelleyWalletAddresses = map (\(ShelleyWallet _ _ addr) -> addr) shelleyWallets
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
        let expTxIns = map (\x -> Tx.txIn (Tx.txId signedFundShelleyWallet) x) [0 .. (length shelleyWalletChunk - 1)]
        mapM_ (\(ti, add) -> verifyTxConfirmation era localNodeConnectInfo add ti) (zip expTxIns shelleyWalletChunk)
        consoleLog ("Batch No." ++ (show (chunkIdx + 1)) ++ " containing " ++ (show (length txOutToShelleyWallet)) ++ " shelley wallets funded with " ++ (show fundValue))
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
        consoleLog ((show (length dRepKeys)) ++ " DReps registered")
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
            committeeHotKeyAuthCerts = map (\(Committee _ _ _ _ _ cert _) -> cert) committee
            committeeColdSkeys = map (\(Committee cold _ _ _ _ _ _) -> C.WitnessCommitteeColdKey cold) committee
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
        consoleLog ((show (length committee)) ++ " Constitutional Committee members registered")
        return Nothing

delegateAdaHolderToDRepsTestInfo adaHolders dreps =
    TestInfo
        { testName = "delegateAdaHolderToDReps"
        , testDescription = "Delegate shelly wallets (ada holders) to DReps"
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
        let adaHolderChunkList = [getChunk 25 0 adaHolders, getChunk 25 1 adaHolders, getChunk 25 2 adaHolders, getChunk 25 3 adaHolders, getChunk 25 4 adaHolders, getChunk 25 5 adaHolders]
            zipwithDreps = zip dReps adaHolderChunkList
        mapM_ (\dRepPairs -> delegateAdaHolderToSelectedDReps dRepPairs networkOptions testParams) zipwithDreps
        consoleLog ("All " ++ (show (length (adaHolders))) ++ " Ada holders delegated to all " ++ (show (length dReps)) ++ " DReps")
        return Nothing

delegateAdaHolderToSelectedDReps ::
    (MonadTest m, MonadIO m) =>
    (DRep era, [ShelleyWallet era]) ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
delegateAdaHolderToSelectedDReps
    dRepPairs
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            (dRep, adaHolderChunk) = dRepPairs
            sKeys = map (ShelleyWallet.stakeSKey) adaHolderChunk
            pKeys = map (ShelleyWallet.paymentSKey) adaHolderChunk
            stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) sKeys
            shelleyWalletAddresses = map (ShelleyWallet.walletAddress) adaHolderChunk
        adaHodlersTxIns <- mapM (\x -> Q.adaOnlyTxInAtAddress era localNodeConnectInfo x) shelleyWalletAddresses
        stakeDelegTxIn1 <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            adaHolderTxOut = map (\x -> Tx.txOut era (C.lovelaceToValue 20_000_000) x) shelleyWalletAddresses
            voteDelgCerts = map (\x -> DRep.voteDelegateCert (toConwayEraOnwards era) (kDRepLedgerCred dRep) x) stakeCreds
            stakeDelegTxBodyContent1 =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns ([stakeDelegTxIn1] ++ adaHodlersTxIns)
                    , C.txCertificates = Tx.txCertificates era voteDelgCerts stakeCreds
                    , C.txOuts = ([stakeDelegTxOut] ++ adaHolderTxOut)
                    }
            adaHolderPaymentWitnesses = map C.WitnessPaymentKey pKeys
            adaHolderStakeWitnesses = map C.WitnessStakeKey sKeys
            totalWitness = fromIntegral (length adaHolderPaymentWitnesses + length adaHolderStakeWitnesses + 1)
        signedStakeDelegTx1 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent1
                w1Address
                (Just totalWitness)
                ([C.WitnessPaymentKey w1SKey] ++ adaHolderPaymentWitnesses ++ adaHolderStakeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx1
        let expTxIn1 = Tx.txIn (Tx.txId signedStakeDelegTx1) 0
        stakeDelegResultTxOut1 <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn1 "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut1
        consoleLog ("Delegated " ++ (show $ length adaHolderChunk) ++ " Ada Holders to DRep: " ++ (show $ kDRepSKey dRep))
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
        let adaHolderChunkList = [getChunk 25 0 adaHolders, getChunk 25 1 adaHolders, getChunk 25 2 adaHolders, getChunk 25 3 adaHolders, getChunk 25 4 adaHolders, getChunk 25 5 adaHolders]
            zipwithPools = zip stakePools adaHolderChunkList
        mapM_ (\poolPairs -> delegateSelectedAdaHolderToStakePool poolPairs networkOptions testParams) zipwithPools
        consoleLog ("All " ++ (show (length adaHolders)) ++ " Ada Holders delegated to all " ++ (show (length stakePools)) ++ " Stake Pools")
        return Nothing

delegateSelectedAdaHolderToStakePool ::
    (MonadTest m, MonadIO m) =>
    (StakePool era, [ShelleyWallet era]) ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe a)
delegateSelectedAdaHolderToStakePool
    stakePoolPairs
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        delegTxIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        let (stakePool, adaHolderChunk) = stakePoolPairs
            shelleyWalletAddresses = map (ShelleyWallet.walletAddress) adaHolderChunk
        adaHodlersTxIns <- mapM (\x -> Q.adaOnlyTxInAtAddress era localNodeConnectInfo x) shelleyWalletAddresses
        let stakeSKeys = map (ShelleyWallet.stakeSKey) adaHolderChunk
            paymentSKeys = map (ShelleyWallet.paymentSKey) adaHolderChunk
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            stakeDelegTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            adaHolderTxOut = map (\x -> Tx.txOut era (C.lovelaceToValue 20_000_000) x) shelleyWalletAddresses
            stakeDelegCerts = map (\x -> stakeDelegCert ceo (sPLedgerKeyHash stakePool) x) stakeCredentials
            stakeDelegTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns ([delegTxIn] ++ adaHodlersTxIns)
                    , C.txCertificates =
                        Tx.txCertificates
                            era
                            stakeDelegCerts
                            stakeCredentials
                    , C.txOuts = ([stakeDelegTxOut] ++ adaHolderTxOut)
                    }
            adaHolderPaymentWitnesses = map C.WitnessPaymentKey paymentSKeys
            adaHolderStakeWitnesses = map C.WitnessStakeKey stakeSKeys
            totalWitness = fromIntegral (length adaHolderPaymentWitnesses + length adaHolderStakeWitnesses + 1)
        signedStakeDelegTx <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                stakeDelegTxBodyContent
                w1Address
                (Just totalWitness)
                ([C.WitnessPaymentKey w1SKey] ++ adaHolderPaymentWitnesses ++ adaHolderStakeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedStakeDelegTx
        let expTxIn = Tx.txIn (Tx.txId signedStakeDelegTx) 0
        stakeDelegResultTxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address expTxIn "getTxOutAtAddress"
        H.annotate $ show stakeDelegResultTxOut
        consoleLog ("Delegated " ++ (show $ length adaHolderChunk) ++ " Ada holders to stake pool: " ++ (show $ sPPoolId stakePool))
        return Nothing

multipleCommitteeProposalAndVoteTestInfo committee dReps shelleyWallets stakePools =
    TestInfo
        { testName = "multipleCommitteeProposalAndVoteTest"
        , testDescription = "Multiple propose and vote on new constitutional committee"
        , test = multipleCommitteeProposalAndVoteTest committee dReps shelleyWallets stakePools
        }

multipleCommitteeProposalAndVoteTest ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleCommitteeProposalAndVoteTest
    committee
    dReps
    shelleyWallets
    stakePools
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2
        -- record the committee before transaction
        oldGovState <- Q.getConwayGovernanceState localNodeConnectInfo
        let oldGovStateJson = C.prettyPrintJSON oldGovState
        oldGovStateFile <- pure $ LBS.writeFile ".govStateTracking/committee/oldGovState.json" (LBS.fromStrict oldGovStateJson)
        liftIO oldGovStateFile
        let
            -- build transaction to propose new committee
            anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/committee.txt"
            anchor = C.createAnchor (fromJust anchorUrl) "new committee"
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        let
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            prevConstitutionalCommittee = []
            newConstitutionalCommittee =
                Map.fromList $
                    map
                        ( \c ->
                            ( L.KeyHashObj $
                                L.KeyHash (fromJust $ L.hashFromBytes $ C.serialiseToRawBytes (committeeHotHash c))
                            , (addEpoch currentEpoch2 1)
                            )
                        )
                        committee
        let quorum = 1 % 2
            proposals =
                map
                    ( \sp ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000 -- govActionDeposit
                            sp
                            (C.ProposeNewCommittee CL.SNothing prevConstitutionalCommittee newConstitutionalCommittee quorum)
                            anchor
                    )
                    stakeCredentials
            txProposals = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\proposal -> (proposal, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposals)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
        H.annotate $ show result1TxOut
        -- vote on committee
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            -- committee member not allowed to vote on committee update
            dRepVoters = map (\d -> (kDRepVoter d, C.Yes, Nothing)) dReps
            spVoters = map (\sp -> (sPVoter sp, C.Yes, Nothing)) stakePools
            -- committeeVoters = map(\cc ->(ccVo) )committee
            votes = dRepVoters ++ spVoters
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
        let tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            spWitnesses = map (\sp -> C.WitnessStakePoolKey $ sPSKey sp) stakePools
            dRepWitnesses = map (\d -> C.WitnessPaymentKey $ DRep.castDRep (kDRepSKey d)) dReps
            totalWitnesses = fromIntegral (length spWitnesses + length dRepWitnesses + 1)
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses) -- witnesses
                ([C.WitnessPaymentKey w1SKey] ++ spWitnesses ++ dRepWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
        H.annotate $ show result2TxOut
        currentEpoch3 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch3"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch3

        currentEpoch4 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch4"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch4

        liftIO $ threadDelay 2_000_000
        newGovState <- Q.getConwayGovernanceState localNodeConnectInfo
        let newEnactState = CG.mkEnactState newGovState
            newCommittee = CG.ensCommittee newEnactState
            newCommitteeMembers = CG.committeeMembers (fromJust $ strictMaybeToMaybe newCommittee)
            committeeChanged = newConstitutionalCommittee == newCommitteeMembers
            newGovStateJson = C.prettyPrintJSON newGovState
        newGovStateFile <- pure $ LBS.writeFile ".govStateTracking/committee/newGovState.json" (LBS.fromStrict newGovStateJson)
        liftIO newGovStateFile
        Helpers.Test.assert "Expected committee updated after voting procedure" committeeChanged

multipleConstitutionProposalAndVotesTestInfo committee dReps shelleyWallet =
    TestInfo
        { testName = "multipleConstitutionProposalAndVotesTest"
        , testDescription = "Multiple constitution Proposal and Vote"
        , test = multipleConstitutionProposalAndVotesTest committee dReps shelleyWallet
        }

multipleConstitutionProposalAndVotesTest ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleConstitutionProposalAndVotesTest
    committee
    dReps
    shelleyWallets
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2

        existingConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
        existingConstitutionHash === "\"0000000000000000000000000000000000000000000000000000000000000000\""

        let constitutionPath = tempAbsPath <> "/constituion.txt"
        H.writeFile constitutionPath "a new way of life"
        constituionBS <- H.evalIO $ BS.readFile constitutionPath
        let
            constituionHash = show (Crypto.hashWith id constituionBS :: Crypto.Hash Crypto.Blake2b_256 BS.ByteString)
            constitutionUrl = fromJust $ (\t -> CL.textToUrl (Text.length t) t) "https://example.com/constituion.txt"
            anchor = C.createAnchor constitutionUrl constituionBS
        H.annotate constituionHash
        oldGovState <- Q.getConwayGovernanceState localNodeConnectInfo
        let oldGovStateJson = C.prettyPrintJSON oldGovState
        oldGovStateFile <- pure $ LBS.writeFile ".govStateTracking/constitution/oldGovState.json" (LBS.fromStrict oldGovStateJson)
        liftIO oldGovStateFile
        -- build a tx to propose constitution
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        let
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            proposals =
                map
                    ( \sp ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000
                            sp
                            (C.ProposeNewConstitution CL.SNothing anchor CL.SNothing)
                            anchor
                    )
                    stakeCredentials
            txProposals =
                C.shelleyBasedEraConstraints sbe $
                    Tx.buildTxProposalProcedures (map (\proposal -> (proposal, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposals)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress1"
        H.annotate $ show result1TxOut
        -- vote on the constitution
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            dRepVote1 = (kDRepVoter (dReps !! 0), C.Yes, Nothing)
            dRepVote2 = (kDRepVoter (dReps !! 1), C.Yes, Nothing)
            dRepVote3 = (kDRepVoter (dReps !! 2), C.No, Nothing)
            committeeVote1 = (committeeVoter (committee !! 0), C.No, Nothing)
            committeeVote2 = (committeeVoter (committee !! 1), C.Yes, Nothing)
            committeeVote3 = (committeeVoter (committee !! 2), C.Yes, Nothing)
            committeeVote4 = (committeeVoter (committee !! 3), C.Yes, Nothing)
            committeeVote = [committeeVote1, committeeVote2, committeeVote3, committeeVote4]
            -- map (\x -> (committeeVoter x, C.No, Nothing)) committee
            dRepVote = [dRepVote1, dRepVote2, dRepVote3]
            -- map (\x -> (kDRepVoter x, C.No, Nothing)) dReps
            votes = dRepVote ++ committeeVote
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
            tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            dRepWitnesses = map (\d -> C.WitnessPaymentKey $ DRep.castDRep $ kDRepSKey d) dReps
            committeeWitnesses = map (\c -> C.WitnessPaymentKey $ CC.castCommittee $ committeeHotSKey c) committee
            totalWitnesses = fromIntegral (length dRepWitnesses + length committeeWitnesses + 1)
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses)
                ([C.WitnessPaymentKey w1SKey] ++ dRepWitnesses ++ committeeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress2"
        H.annotate $ show result2TxOut
        -- wait for next epoch before asserting for new constitution
        currentEpoch3 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch3"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch3

        currentEpoch4 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch4"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch4

        -- wait 2 seconds at start of epoch to account for any delay with constitution enactment
        liftIO $ threadDelay 2_000_000
        -- check new constituion is enacted
        afterDelay <- Q.getConwayGovernanceState localNodeConnectInfo
        let afterDelayJson = C.prettyPrintJSON afterDelay
        afterDelayFile <- pure $ LBS.writeFile ".govStateTracking/constitution/newGovState.json" (LBS.fromStrict afterDelayJson)
        liftIO afterDelayFile
        newConstitutionHash <- Q.getConstitutionAnchorHashAsString era localNodeConnectInfo
        consoleLog ("constituionHash: " ++ show constituionHash)
        consoleLog ("newConstitutionHash: " ++ show newConstitutionHash)
        Helpers.Test.assert "expected constitution hash matches query result" (constituionHash == newConstitutionHash)

multipleNoConfidenceProposalAndVoteTestInfo dReps shelleyWallets stakePools =
    TestInfo
        { testName = "multipleNoConfidenceProposalAndVoteTest"
        , testDescription = "Mulriple propose and vote on a motion of no-confidence"
        , test = multipleNoConfidenceProposalAndVoteTest dReps shelleyWallets stakePools
        }

multipleNoConfidenceProposalAndVoteTest ::
    (MonadTest m, MonadIO m) =>
    [DRep era] ->
    [ShelleyWallet era] ->
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleNoConfidenceProposalAndVoteTest
    dReps
    shelleyWallets
    stakePools
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        case sbe of
            C.ShelleyBasedEraConway -> do
                currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
                H.annotate $ show currentEpoch1
                currentEpoch2 <-
                    Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                        =<< Q.getCurrentEpoch era localNodeConnectInfo
                H.annotate $ show currentEpoch2
                let anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/no_confidence.txt"
                    anchor = C.createAnchor (fromJust anchorUrl) "motion of no confidence"
                tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
                oldGovState <- Q.getConwayGovernanceState localNodeConnectInfo
                randomShelleyWallets <- pickRandomElements 5 shelleyWallets
                let
                    previousGovActions = getPrevGovAction oldGovState
                    prevUpdateCommitted = udpateCommittee previousGovActions
                let
                    tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
                    tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
                    stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
                    stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
                    proposals =
                        map
                            ( \sp ->
                                C.createProposalProcedure
                                    sbe
                                    (C.toShelleyNetwork networkId)
                                    1_000_000
                                    sp
                                    (C.MotionOfNoConfidence prevUpdateCommitted)
                                    anchor
                            )
                            stakeCredentials
                    txProposal = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\p -> (p, Nothing)) proposals)
                    tx1BodyContent =
                        (Tx.emptyTxBodyContent sbe pparams)
                            { C.txIns = Tx.pubkeyTxIns [tx1In]
                            , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposal)
                            , C.txOuts = [tx1Out1, tx1Out2]
                            }
                signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
                Tx.submitTx sbe localNodeConnectInfo signedTx1
                let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
                    _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
                    tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
                result1TxOut <-
                    Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
                H.annotate $ show result1TxOut
                -- vote on the motion of no-confidence
                let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
                    -- committee not allowed to vote on motion of no-confidence
                    spVotes = map (\sp -> (sPVoter sp, C.No, Nothing)) stakePools
                    dRepVotes = map (\d -> (kDRepVoter d, C.No, Nothing)) dReps
                    votes = spVotes ++ dRepVotes
                    txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
                let tx2BodyContent =
                        (Tx.emptyTxBodyContent sbe pparams)
                            { C.txIns = Tx.pubkeyTxIns [tx2In3]
                            , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                            , C.txOuts = [tx2Out1]
                            }
                    spWitnesses = map (\sp -> C.WitnessStakePoolKey (sPSKey sp)) stakePools
                    dRepWitnesses = map (\d -> C.WitnessPaymentKey (DRep.castDRep $ kDRepSKey d)) dReps
                    totalWitnesses = fromIntegral (length spWitnesses + length dRepWitnesses + 1)
                signedTx2 <-
                    Tx.buildTxWithWitnessOverride
                        era
                        localNodeConnectInfo
                        tx2BodyContent
                        w1Address
                        (Just totalWitnesses) -- witnesses
                        ([C.WitnessPaymentKey w1SKey] ++ spWitnesses ++ dRepWitnesses)
                Tx.submitTx sbe localNodeConnectInfo signedTx2
                let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
                result2TxOut <-
                    Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
                H.annotate $ show result2TxOut
                currentEpoch3 <-
                    Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch3"
                        =<< Q.getCurrentEpoch era localNodeConnectInfo
                H.annotate $ show currentEpoch3

                currentEpoch4 <-
                    Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch4"
                        =<< Q.getCurrentEpoch era localNodeConnectInfo
                H.annotate $ show currentEpoch4
                liftIO $ threadDelay 2_000_000
                newGovState <- Q.getConwayGovernanceState localNodeConnectInfo
                let oldGovStateJson = C.prettyPrintJSON oldGovState
                    newGovStateJson = C.prettyPrintJSON newGovState
                oldGovStateFile <- pure $ LBS.writeFile ".govStateTracking/noConfidence/oldGovState.json" (LBS.fromStrict oldGovStateJson)
                newGovStateFile <- pure $ LBS.writeFile ".govStateTracking/noConfidence/newGovState.json" (LBS.fromStrict newGovStateJson)
                liftIO oldGovStateFile
                liftIO newGovStateFile
                Helpers.Test.assert "Gov State should not change" (newGovState == oldGovState)
            _ -> error "Expected Test to run in Conway Era"

multiplePrameterChangeProposalAndVoteTestInfo committee dReps shelleyWallets =
    TestInfo
        { testName = "multiplePrameterChangeProposalAndVoteTest"
        , testDescription = "Multiple propose and vote on a change to the protocol parameters"
        , test = multiplePrameterChangeProposalAndVoteTest committee dReps shelleyWallets
        }

multiplePrameterChangeProposalAndVoteTest ::
    (MonadTest m, MonadIO m, L.ConwayEraPParams (C.ShelleyLedgerEra era)) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multiplePrameterChangeProposalAndVoteTest
    committee
    dReps
    shelleyWallets
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2
        let anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/pparameters.txt"
            anchor = C.createAnchor (fromJust anchorUrl) "protocol parameters"
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        let
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            pparamsUpdate = L.emptyPParamsUpdate & L.ppuCommitteeMinSizeL .~ CL.SJust (1 :: Natural)
            proposals =
                map
                    ( \sp ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000 -- govActionDeposit
                            sp
                            (C.UpdatePParams CL.SNothing pparamsUpdate CL.SNothing)
                            anchor
                    )
                    stakeCredentials
            txProposal = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\p -> (p, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposal)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
        H.annotate $ show result1TxOut
        -- vote
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            -- SPO not allowed to vote on protocol parameters update
            dRepVotes = map (\d -> (kDRepVoter d, C.No, Nothing)) dReps
            committeeVotes = map (\c -> (committeeVoter c, C.No, Nothing)) committee
            votes = dRepVotes ++ committeeVotes
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
        let tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            dRepWitnesses = map (\d -> C.WitnessPaymentKey (DRep.castDRep $ kDRepSKey d)) dReps
            committeeWitnesses = map (\c -> C.WitnessPaymentKey (CC.castCommittee $ committeeHotSKey c)) committee
            totalWitnesses = fromIntegral (length dRepWitnesses + length committeeWitnesses + 1)
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses) -- witnesses
                ([C.WitnessPaymentKey w1SKey] ++ dRepWitnesses ++ committeeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
        H.annotate $ show result2TxOut
        return Nothing

multipleTreasuryWithdrawalProposalAndVoteTestInfo committee dRep shelleyWallets =
    TestInfo
        { testName = "multipleTreasuryWithdrawalProposalAndVoteTest"
        , testDescription = "Multiple propose and vote on a treasury withdrawal"
        , test = multipleTreasuryWithdrawalProposalAndVoteTest committee dRep shelleyWallets
        }

multipleTreasuryWithdrawalProposalAndVoteTest ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleTreasuryWithdrawalProposalAndVoteTest
    committee
    dReps
    shelleyWallets
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2
        let anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/treasury_withdrawal.txt"
            anchor = C.createAnchor (fromJust anchorUrl) "treasury withdrawal"
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        let
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            tWithdrawal = map (\sc -> (CL.Testnet, sc, 10_000_000 :: L.Coin)) stakeCredentials
            proposals =
                map
                    ( \sp ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000 -- govActionDeposit
                            sp
                            (C.TreasuryWithdrawal tWithdrawal CL.SNothing) -- SNothing is Governance policy
                            anchor
                    )
                    stakeCredentials
            txProposal = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\p -> (p, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposal)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
        H.annotate $ show result1TxOut
        -- vote on the treasury withdrawal
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            -- SPO not allowed to vote on treasury withdrawal
            dRepVotes = map (\d -> (kDRepVoter d, C.Yes, Nothing)) dReps
            committeeVotes = map (\c -> (committeeVoter c, C.Yes, Nothing)) committee
            votes = dRepVotes ++ committeeVotes
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
        let tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            dRepWitnesses = map (\d -> C.WitnessPaymentKey (DRep.castDRep $ kDRepSKey d)) dReps
            committeeWitnesses = map (\c -> C.WitnessPaymentKey (CC.castCommittee $ committeeHotSKey c)) committee
            totalWitnesses = fromIntegral $ (length $ dRepWitnesses ++ committeeWitnesses) + 1
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses) -- witnesses
                ([C.WitnessPaymentKey w1SKey] ++ dRepWitnesses ++ committeeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
        H.annotate $ show result2TxOut
        return Nothing

mkProtocolVersionOrErr :: (Natural, Natural) -> L.ProtVer
mkProtocolVersionOrErr (majorProtVer, minorProtVer) =
    case (`L.ProtVer` minorProtVer) <$> L.mkVersion majorProtVer of
        Just v -> v
        Nothing ->
            error $ "mkProtocolVersionOrErr: invalid protocol version " <> show (majorProtVer, minorProtVer)

multipleHardForkProposalAndVoteTestInfo committee dReps shelleyWallet stakePools =
    TestInfo
        { testName = "multipleHardForkProposalAndVoteTest"
        , testDescription = "Multiple Propose and vote on a hard fork"
        , test = multipleHardForkProposalAndVoteTest committee dReps shelleyWallet stakePools
        }

multipleHardForkProposalAndVoteTest ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleHardForkProposalAndVoteTest
    committee
    dReps
    shelleyWallets
    stakePools
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2
        -- create proposal
        let anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/hard_fork.txt"
            anchor = C.createAnchor (fromJust anchorUrl) "hard fork"
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        pvNat :: Natural <- toEnum <$> TN.pvFromOptions networkOptions
        let
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            nextPv = mkProtocolVersionOrErr (pvNat, 1)
            proposals =
                map
                    ( \sp ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000 -- govActionDeposit
                            sp
                            (C.InitiateHardfork CL.SNothing nextPv)
                            anchor
                    )
                    stakeCredentials
            txProposal = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\p -> (p, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposal)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
        H.annotate $ show result1TxOut
        -- vote
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            committeeVotes = map (\c -> (committeeVoter c, C.No, Nothing)) committee
            dRepVotes = map (\d -> (kDRepVoter d, C.No, Nothing)) dReps
            spVotes = map (\sp -> (sPVoter sp, C.No, Nothing)) stakePools
            votes = committeeVotes ++ dRepVotes ++ spVotes
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
            tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            stakePoolWitnesses = map (\sp -> C.WitnessStakePoolKey (sPSKey sp)) stakePools
            dRepWitnesses = map (\d -> C.WitnessPaymentKey (DRep.castDRep (kDRepSKey d))) dReps
            committeeWitnesses = map (\c -> C.WitnessPaymentKey (CC.castCommittee (committeeHotSKey c))) committee
            totalWitnesses = fromIntegral $ (length $ stakePoolWitnesses ++ dRepWitnesses ++ committeeWitnesses) + 1
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses) -- witnesses
                ([C.WitnessPaymentKey w1SKey] ++ stakePoolWitnesses ++ dRepWitnesses ++ committeeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
        H.annotate $ show result2TxOut
        return Nothing

multipleInfoProposalAndVoteTestInfo committee dReps shelleyWallet stakePools =
    TestInfo
        { testName = "multipleInfoProposalAndVoteTest"
        , testDescription = "Multiple propose and vote on an Info action"
        , test = multipleInfoProposalAndVoteTest committee dReps shelleyWallet stakePools
        }

multipleInfoProposalAndVoteTest ::
    (MonadTest m, MonadIO m) =>
    [Committee era] ->
    [DRep era] ->
    [ShelleyWallet era] ->
    [StakePool era] ->
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
multipleInfoProposalAndVoteTest
    committee
    dReps
    shelleyWallets
    stakePools
    networkOptions
    TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
        era <- TN.eraFromOptionsM networkOptions
        skeyAndAddress <- TN.w tempAbsPath networkId
        let (w1SKey, _, w1Address) = skeyAndAddress !! 0
            sbe = toShelleyBasedEra era
            ceo = toConwayEraOnwards era
        currentEpoch1 <- Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch1
        currentEpoch2 <-
            Q.waitForNextEpoch era localNodeConnectInfo "currentEpoch2"
                =<< Q.getCurrentEpoch era localNodeConnectInfo
        H.annotate $ show currentEpoch2
        let anchorUrl = (\t -> CL.textToUrl (Text.length t) t) "https://example.com/info.txt"
            anchor = C.createAnchor (fromJust anchorUrl) "Info"
        tx1In <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
        randomShelleyWallets <- pickRandomElements 5 shelleyWallets
        let
            tx1Out1 = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            tx1Out2 = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
            stakeSKeys = map (\(ShelleyWallet skey _ _) -> skey) randomShelleyWallets
            stakeCredentials = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
            proposals =
                map
                    ( \sc ->
                        C.createProposalProcedure
                            sbe
                            (C.toShelleyNetwork networkId)
                            1_000_000 -- govActionDeposit
                            sc
                            C.InfoAct
                            anchor
                    )
                    stakeCredentials
            txProposal = C.shelleyBasedEraConstraints sbe $ Tx.buildTxProposalProcedures (map (\p -> (p, Nothing)) proposals)
            tx1BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx1In]
                    , C.txProposalProcedures = C.forEraInEonMaybe era (`C.Featured` txProposal)
                    , C.txOuts = [tx1Out1, tx1Out2]
                    }
        signedTx1 <- Tx.buildTx era localNodeConnectInfo tx1BodyContent w1Address [w1SKey]
        Tx.submitTx sbe localNodeConnectInfo signedTx1
        let _tx2In1@(C.TxIn tx2InId1 _tx2InIx1) = Tx.txIn (Tx.txId signedTx1) 0
            _tx2In2 = Tx.txIn (Tx.txId signedTx1) 1
            tx2In3 = Tx.txIn (Tx.txId signedTx1) 2 -- change output
        result1TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address tx2In3 "getTxOutAtAddress"
        H.annotate $ show result1TxOut
        -- vote
        let tx2Out1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w1Address
            committeeVotes = map (\c -> (committeeVoter c, C.No, Nothing)) committee
            dRepVotes = map (\d -> (kDRepVoter d, C.No, Nothing)) dReps
            spVotes = map (\sp -> (sPVoter sp, C.No, Nothing)) stakePools
            votes = committeeVotes ++ dRepVotes ++ spVotes
            txVotingProcedures = Tx.buildTxVotingProcedures sbe ceo tx2InId1 0 votes
            tx2BodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [tx2In3]
                    , C.txVotingProcedures = C.forEraInEonMaybe era (`C.Featured` txVotingProcedures)
                    , C.txOuts = [tx2Out1]
                    }
            stakePoolWitnesses = map (\sp -> C.WitnessStakePoolKey (sPSKey sp)) stakePools
            dRepWitnesses = map (\d -> C.WitnessPaymentKey (DRep.castDRep (kDRepSKey d))) dReps
            committeeWitnesses = map (\c -> C.WitnessPaymentKey (CC.castCommittee (committeeHotSKey c))) committee
            totalWitnesses = fromIntegral $ (length $ stakePoolWitnesses ++ dRepWitnesses ++ committeeWitnesses) + 1
        signedTx2 <-
            Tx.buildTxWithWitnessOverride
                era
                localNodeConnectInfo
                tx2BodyContent
                w1Address
                (Just totalWitnesses) -- witnesses
                ([C.WitnessPaymentKey w1SKey] ++ stakePoolWitnesses ++ dRepWitnesses ++ committeeWitnesses)
        Tx.submitTx sbe localNodeConnectInfo signedTx2
        let result2TxIn = Tx.txIn (Tx.txId signedTx2) 0
        result2TxOut <-
            Q.getTxOutAtAddress era localNodeConnectInfo w1Address result2TxIn "getTxOutAtAddress"
        H.annotate $ show result2TxOut
        return Nothing
