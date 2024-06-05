{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.PlutusTests where

import Cardano.Api qualified as C
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Hedgehog hiding (test)
import Helpers.Common (toShelleyBasedEra)
import Helpers.PlutusScripts (mintScriptWitness', plutusL3, spendScriptWitness)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters (fromCardanoTxIn, toPlutusAddress, toPlutusValue)
import Test.V3.DummyDataTypes
import Utils
import V2.Mint.VerifyMintingMaxExUnits qualified
import V3.Mint.VerifyMintingMaxExUnits qualified
import V3.Spend.SimpleScript qualified
import V3.Spend.VerifyBLS12G1 qualified
import V3.Spend.VerifyBLS12G2 qualified
import V3.Spend.VerifyBlake2b224 qualified
import V3.Spend.VerifyEcdsa qualified
import V3.Spend.VerifyEd25519 qualified
import V3.Spend.VerifyKeccak qualified
import V3.Spend.VerifyMultiSig qualified
import V3.Spend.VerifyRefInputVisibility qualified
import V3.Spend.VerifySchnorr qualified

verifyEcdsaSignatureForUtxoUnlockingTestInfo :: TestInfo era
verifyEcdsaSignatureForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifyEcdsaSignatureForUtxoUnlockingTest"
        , testDescription =
            "Verify verifyEcdsaSecp256k1Signature function for unlocking funds"
        , test = verifyEcdsaSignatureForUtxoUnlockingTest
        }

verifyEcdsaSignatureForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyEcdsaSignatureForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyEcdsaInfo = v3ScriptInfo networkId V3.Spend.VerifyEcdsa.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyEcdsaInfo)
                (dataToHashableScriptData v3VerifyEcdsaDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyEcdsaInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (v3sbs verifyEcdsaInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyEcdsaRedeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 2_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyEd25519SignatureForUtxoUnlockingTestInfo :: TestInfo era
verifyEd25519SignatureForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifyEd25519SignatureForUtxoUnlockingTest"
        , testDescription =
            "Verify verifyEd25519Signature function for unlocking funds"
        , test = verifyEd25519SignatureForUtxoUnlockingTest
        }

verifyEd25519SignatureForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyEd25519SignatureForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyEd25519Info = v3ScriptInfo networkId V3.Spend.VerifyEd25519.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyEd25519Info)
                (dataToHashableScriptData v3VerifyEd25519Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyEd25519Info) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (v3sbs verifyEd25519Info))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyEd25519Datum)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 2_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifySchnorrSignatureForUtxoUnlockingTestInfo :: TestInfo era
verifySchnorrSignatureForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifySchnorrSignatureForUtxoUnlockingTest"
        , testDescription =
            "Verify verifySchnorrSecp256k1Signature function for unlocking funds"
        , test = verifySchnorrSignatureForUtxoUnlockingTest
        }

verifySchnorrSignatureForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifySchnorrSignatureForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifySchnorrInfo = v3ScriptInfo networkId V3.Spend.VerifySchnorr.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifySchnorrInfo)
                (dataToHashableScriptData v3VerifySchnorrDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifySchnorrInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (v3sbs verifySchnorrInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifySchnorrRedeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 2_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyKeccak256ForUtxoUnlockingTestInfo :: TestInfo era
verifyKeccak256ForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifyKeccak256ForUtxoUnlockingTest"
        , testDescription =
            "Verify keccak256 function for validating ECDSA signature formatted via the EVM standard."
        , test = verifyKeccak256ForUtxoUnlockingTest
        }

verifyKeccak256ForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyKeccak256ForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyKeccakInfo = v3ScriptInfo networkId (V3.Spend.VerifyKeccak.validator v3VerifyKeccakParameter)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyKeccakInfo)
                (dataToHashableScriptData v3VerifyKeccakDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyKeccakInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs verifyKeccakInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyKeccakRedeemer)
        collateral = Tx.txInsCollateral era [txIn]
        -- TODO: add signer as extra witness to the transaction
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyBLS12G1ForUtxoUnlockingTestInfo :: TestInfo era
verifyBLS12G1ForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifyBLS12G1ForUtxoUnlockingTest"
        , testDescription =
            "Verify bls12_381_G1 functions for unlocking funds from a script."
        , test = verifyBLS12G1ForUtxoUnlockingTest
        }

verifyBLS12G1ForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyBLS12G1ForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBLSInfo = v3ScriptInfo networkId (V3.Spend.VerifyBLS12G1.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyBLSInfo)
                (dataToHashableScriptData blsG1Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyBLSInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs verifyBLSInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData blsG1Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyBLS12G2ForUtxoUnlockingTestInfo :: TestInfo era
verifyBLS12G2ForUtxoUnlockingTestInfo =
    TestInfo
        { testName = "verifyBLS12G2ForUtxoUnlockingTest"
        , testDescription =
            "Verify bls12_381_G1 functions for unlocking funds from a script."
        , test = verifyBLS12G2ForUtxoUnlockingTest
        }

verifyBLS12G2ForUtxoUnlockingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyBLS12G2ForUtxoUnlockingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBLSInfo = v3ScriptInfo networkId (V3.Spend.VerifyBLS12G2.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyBLSInfo)
                (dataToHashableScriptData blsG2Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyBLSInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs verifyBLSInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData blsG2Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyBlake2b224ForValidatingPubKeyHashTestInfo :: TestInfo era
verifyBlake2b224ForValidatingPubKeyHashTestInfo =
    TestInfo
        { testName = "verifyBlake2b224ForValidatingPubKeyHashTest"
        , testDescription =
            "Verify blake2b_224 function for validating pubKeyHash."
        , test = verifyBlake2b224ForValidatingPubKeyHashTest
        }

verifyBlake2b224ForValidatingPubKeyHashTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyBlake2b224ForValidatingPubKeyHashTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBlake2b224Info = v3ScriptInfo networkId (V3.Spend.VerifyBlake2b224.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyBlake2b224Info)
                (dataToHashableScriptData v3VerifyBlake2b224Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyBlake2b224Info) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs verifyBlake2b224Info))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyBlake2b224Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyReferenceInputVisibilityTestInfo :: TestInfo era
verifyReferenceInputVisibilityTestInfo =
    TestInfo
        { testName = "verifyReferenceInputVisibilityTest"
        , testDescription =
            "Verify visibility of reference input's datum, value and address to unlock a UTxO."
        , test = verifyReferenceInputVisibilityTest
        }

verifyReferenceInputVisibilityTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyReferenceInputVisibilityTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
        (w2SKey, _, w2Address) = skeyAndAddress !! 1
        sbe = toShelleyBasedEra era
    txIn <- Q.firstTxIn era localNodeConnectInfo w1Address
    let collateral = Tx.txInsCollateral era [txIn]
        datum42 :: Integer = 42
        createRefInputUTxO =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                w2Address
                (dataToHashableScriptData datum42)
        fundRefInput =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [createRefInputUTxO]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundRefInput w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let refTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address refTxIn "TN.getTxOutAtAddress"
    let datumAddress =
            toPlutusAddress $
                C.anyAddressInShelleyBasedEra ShelleyBasedEraConway (C.toAddressAny w2Address)
        datumValue = foldMap toPlutusValue valueList
          where
            valueList =
                map
                    (\(C.TxOut _ (C.TxOutValueShelleyBased sbe v) _ _) -> C.fromLedgerValue sbe v)
                    [resultTxOut]
        datum = V3.Spend.VerifyRefInputVisibility.RefInputConfig datumAddress datumValue datum42
    -- contract details
    txIn <- Q.firstTxIn era localNodeConnectInfo w1Address
    let collateral = Tx.txInsCollateral era [txIn]
        verifyRefInputVisibilityInfo = v3ScriptInfo networkId (V3.Spend.VerifyRefInputVisibility.validator)
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address verifyRefInputVisibilityInfo)
                (dataToHashableScriptData datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address verifyRefInputVisibilityInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w1Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs verifyRefInputVisibilityInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData ())
        collateral = Tx.txInsCollateral era [txIn]
        referenceInput = Tx.txInsReference era [refTxIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                , C.txInsReference = referenceInput
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyMaxExUnitsMintingTestInfo :: TestInfo era
verifyMaxExUnitsMintingTestInfo =
    TestInfo
        { testName = "verifyMaxExUnitsMintingTest"
        , testDescription =
            "Verify minting can be done with exactly max execution units."
        , test = verifyMaxExUnitsMintingTest
        }

verifyMaxExUnitsMintingTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMaxExUnitsMintingTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    pv <- TN.pvFromOptions networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
        sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    txInAsTxOut@(C.TxOut _ txInValue _ _) <-
        Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"
    let v3ScriptValidator = V3.Mint.VerifyMintingMaxExUnits.validator (fromCardanoTxIn txIn)
        v2ScriptValidator = V2.Mint.VerifyMintingMaxExUnits.validator (fromCardanoTxIn txIn)
        verifyMaxExUnitsMintingInfoV3 = v3ScriptInfo networkId v3ScriptValidator
        verifyMaxExUnitsMintingInfoV2 = v2ScriptInfo networkId v2ScriptValidator
        policyId = C.PolicyId $ v3hash verifyMaxExUnitsMintingInfoV3
        assetId = C.AssetId policyId "MaxExUnitsMint"
        tokenValues = C.valueFromList [(assetId, 1)]
        collateral = Tx.txInsCollateral era [txIn]
        totalLovelace = C.txOutValueToLovelace txInValue
        amountPaid = 4_000_000
        fee = 2_500_000 :: C.Lovelace
        amountReturned = totalLovelace - amountPaid - fee
        maxExecutionUnits =
            C.ExecutionUnits
                { C.executionSteps = 10_000_000_000
                , C.executionMemory = 14_000_000
                }
        txOut = Tx.txOut era (C.lovelaceToValue 4_000_000 <> tokenValues) w1Address
        txOutChange = Tx.txOut era (C.lovelaceToValue amountReturned) w1Address
        mintWitness =
            Map.fromList
                [
                    ( policyId
                    , mintScriptWitness'
                        sbe
                        plutusL3
                        (Left $ PlutusScriptSerialised (v3sbs verifyMaxExUnitsMintingInfoV3))
                        (dataToHashableScriptData ())
                        maxExecutionUnits
                    )
                ]
        txBodyConntent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txMintValue = Tx.txMintValue era tokenValues mintWitness
                , C.txFee = Tx.txFee era fee
                , C.txOuts = [txOut, txOutChange]
                }
    builtTx <- Tx.buildRawTx sbe txBodyConntent
    rawTx <- Tx.signTx sbe builtTx (C.WitnessPaymentKey w1SKey)
    let signedTx = C.makeSignedTransaction [rawTx] builtTx
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let mintedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w1Address mintedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000 <> tokenValues)
    let efficiency = v3sbs verifyMaxExUnitsMintingInfoV3 < v2sbs verifyMaxExUnitsMintingInfoV2
    Helpers.Test.assert "Tokens Minted" (txOutHasValue && efficiency)

-- locking multiple UTxOs in the same script address in the same transaction
-- spending multiple UTxOs from the same sctipt address in the same transaction
verifyLockingAndSpendingInSameScriptTestInfo :: TestInfo era
verifyLockingAndSpendingInSameScriptTestInfo =
    TestInfo
        { testName = "verifyLockingAndSpendingInSameScriptTest"
        , testDescription =
            "Verify locking and spending multiple UTxOs in/from the same script address in the same transaction."
        , test = verifyLockingAndSpendingInSameScriptTest
        }

verifyLockingAndSpendingInSameScriptTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyLockingAndSpendingInSameScriptTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
        sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let collateral = Tx.txInsCollateral era [txIn]
        paramRedeemer :: Integer = 10046737
        simpleScriptInfo = v3ScriptInfo networkId (V3.Spend.SimpleScript.validator paramRedeemer)
        scriptTxOut1 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address simpleScriptInfo)
                (dataToHashableScriptData ())
        scriptTxOut2 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 3_000_000)
                (v3address simpleScriptInfo)
                (dataToHashableScriptData ())
        scriptTxOut3 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 2_000_000)
                (v3address simpleScriptInfo)
                (dataToHashableScriptData ())
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut1, scriptTxOut2, scriptTxOut3]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn1 = Tx.txIn (Tx.txId signedTx) 0
        scriptTxIn2 = Tx.txIn (Tx.txId signedTx) 1
        scriptTxIn3 = Tx.txIn (Tx.txId signedTx) 2
    resultTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo) scriptTxIn1 "TN.getTxOutAtAddress"
    txOutHasValue1 <- Q.txOutHasValue resultTxOut1 (C.lovelaceToValue 4_000_000)
    resultTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo) scriptTxIn2 "TN.getTxOutAtAddress"
    txOutHasValue2 <- Q.txOutHasValue resultTxOut2 (C.lovelaceToValue 3_000_000)
    resultTxOut3 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo) scriptTxIn3 "TN.getTxOutAtAddress"
    txOutHasValue3 <- Q.txOutHasValue resultTxOut3 (C.lovelaceToValue 2_000_000)
    Helpers.Test.assert "Script has been funded" (txOutHasValue1 && txOutHasValue2 && txOutHasValue3)
    -- redeem 2 UtxOs from script and fund 2 UtxOs to sctipt in the same transaction
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let collateral = Tx.txInsCollateral era [txIn]
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs simpleScriptInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData paramRedeemer)
        redeemTxOut1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w2Address
        redeemTxOut2 = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptTxOut4 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 5_000_000)
                (v3address simpleScriptInfo)
                (dataToHashableScriptData ())
        scriptTxOut5 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 6_000_000)
                (v3address simpleScriptInfo)
                (dataToHashableScriptData ())
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns =
                    (Tx.scriptTxIn [scriptTxIn1, scriptTxIn2] scriptWitness)
                        ++ (Tx.pubkeyTxIns [txIn])
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut1, redeemTxOut2, scriptTxOut4, scriptTxOut5]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn1 = Tx.txIn (Tx.txId signedTx) 0
        redeemedTxIn2 = Tx.txIn (Tx.txId signedTx) 1
        scriptTxIn4 = Tx.txIn (Tx.txId signedTx) 2
        scriptTxIn5 = Tx.txIn (Tx.txId signedTx) 3
    walletTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn1 "TN.getTxOutAtAddress"
    walletTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn2 "TN.getTxOutAtAddress"
    scriptTxOut4 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo) scriptTxIn4 "TN.getTxOutAtAddress"
    scriptTxOut5 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo) scriptTxIn5 "TN.getTxOutAtAddress"
    walletTxOutHasValue1 <- Q.txOutHasValue walletTxOut1 (C.lovelaceToValue 4_000_000)
    walletTxOutHasValue2 <- Q.txOutHasValue walletTxOut2 (C.lovelaceToValue 2_000_000)
    scriptTxOutHasValue4 <- Q.txOutHasValue scriptTxOut4 (C.lovelaceToValue 5_000_000)
    scriptTxOutHasValue5 <- Q.txOutHasValue scriptTxOut5 (C.lovelaceToValue 6_000_000)
    Helpers.Test.assert "Funds Locked and Unlocked" (walletTxOutHasValue1 && walletTxOutHasValue2 && scriptTxOutHasValue4 && scriptTxOutHasValue5)

-- locking utxos to different script address in the same transaction
-- redeeming utxos from different script addresses in the same transaction
verifyLockingAndSpendingInDifferentScriptTestInfo :: TestInfo era
verifyLockingAndSpendingInDifferentScriptTestInfo =
    TestInfo
        { testName = "verifyLockingAndSpendingInDifferentScriptTest"
        , testDescription =
            "Verify locking and spending multiple UTxOs in/from different script address in the same transaction."
        , test = verifyLockingAndSpendingInDifferentScriptTest
        }

verifyLockingAndSpendingInDifferentScriptTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyLockingAndSpendingInDifferentScriptTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, _, w1Address) = skeyAndAddress !! 0
        sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let collateral = Tx.txInsCollateral era [txIn]
        paramRedeemer1 :: Integer = 9848447
        paramRedeemer2 :: Integer = 1635243
        simpleScriptInfo1 = v3ScriptInfo networkId (V3.Spend.SimpleScript.validator paramRedeemer1)
        simpleScriptInfo2 = v3ScriptInfo networkId (V3.Spend.SimpleScript.validator paramRedeemer2)
        script1TxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (v3address simpleScriptInfo1)
                (dataToHashableScriptData ())
        script2TxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 5_000_000)
                (v3address simpleScriptInfo2)
                (dataToHashableScriptData ())
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [script1TxOut, script2TxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let script1TxIn = Tx.txIn (Tx.txId signedTx) 0
        script2TxIn = Tx.txIn (Tx.txId signedTx) 1
    scriptTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo1) script1TxIn "TN.getTxOutAtAddress"
    scriptTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address simpleScriptInfo2) script2TxIn "TN.getTxOutAtAddress"
    scriptTxOut1HasValue <- Q.txOutHasValue scriptTxOut1 (C.lovelaceToValue 4_000_000)
    scriptTxOut2HasValue <- Q.txOutHasValue scriptTxOut2 (C.lovelaceToValue 5_000_000)
    Helpers.Test.assert "Scripts has been funded" (scriptTxOut1HasValue && scriptTxOut2HasValue)
    -- redeem from both scripts in the same transaction
    let (w2SKey, _, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let collateral = Tx.txInsCollateral era [txIn]
        scriptWitness1 =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs simpleScriptInfo1))
                    C.InlineScriptDatum
                    (dataToHashableScriptData paramRedeemer1)
        scriptWitness2 =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs simpleScriptInfo2))
                    C.InlineScriptDatum
                    (dataToHashableScriptData paramRedeemer2)
        redeemTxOut1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w2Address
        redeemTxOut2 = Tx.txOut era (C.lovelaceToValue 5_000_000) w2Address
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns =
                    (Tx.scriptTxIn [script1TxIn] scriptWitness1)
                        ++ (Tx.scriptTxIn [script2TxIn] scriptWitness2)
                        ++ (Tx.pubkeyTxIns [txIn])
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut1, redeemTxOut2]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn1 = Tx.txIn (Tx.txId signedTx) 0
        redeemedTxIn2 = Tx.txIn (Tx.txId signedTx) 1
    walletTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn1 "TN.getTxOutAtAddress"
    walletTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn2 "TN.getTxOutAtAddress"
    walletTxOutHasValue1 <- Q.txOutHasValue walletTxOut1 (C.lovelaceToValue 4_000_000)
    walletTxOutHasValue2 <- Q.txOutHasValue walletTxOut2 (C.lovelaceToValue 5_000_000)
    Helpers.Test.assert "Funds Unlocked" (walletTxOutHasValue1 && walletTxOutHasValue2)

verifyMultiSigRequirementTestInfo :: TestInfo era
verifyMultiSigRequirementTestInfo =
    TestInfo
        { testName = "verifyMultiSigRequirementTest"
        , testDescription =
            "Verify spending UTxOs from script address requiring multiple signatures."
        , test = verifyMultiSigRequirementTest
        }

verifyMultiSigRequirementTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyMultiSigRequirementTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, v1SKey, w1Address) = skeyAndAddress !! 0
        (w2SKey, v2SKey, w2Address) = skeyAndAddress !! 1
        (w3SKey, v3SKey, w3Address) = skeyAndAddress !! 2
        sbe = toShelleyBasedEra era
    let
        pubKeyHashes =
            map
                fromJust
                (map C.shelleyPayAddrToPlutusPubKHash [w1Address, w2Address, w3Address])
        threshold = 2
        parameter = V3.Spend.VerifyMultiSig.MultiSigParams pubKeyHashes threshold
        multisigScriptInfo = v3ScriptInfo networkId (V3.Spend.VerifyMultiSig.validator parameter)
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let w1Collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 50_000_000)
                (v3address multisigScriptInfo)
                (dataToHashableScriptData ())
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = w1Collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    scriptTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address multisigScriptInfo) scriptTxIn "TN.getTxOutAtAddress"
    scriptTxOutHasValue <- Q.txOutHasValue scriptTxOut (C.lovelaceToValue 50_000_000)
    Helpers.Test.assert "Scripts has been funded" scriptTxOutHasValue
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let w2Collateral = Tx.txInsCollateral era [txIn]
        signers = Tx.txExtraKeyWits era [v2SKey, v3SKey]
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs multisigScriptInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData ())
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 30_000_000) w3Address
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = w2Collateral
                , C.txOuts = [redeemTxOut]
                , C.txExtraKeyWits = signers
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Address [w2SKey, w3SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    w3TxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w3Address redeemedTxIn "TN.getTxOutAtAddress"
    w3TxOutHasValue <- Q.txOutHasValue w3TxOut (C.lovelaceToValue 30_000_000)
    Helpers.Test.assert "Funds Unlocked" w3TxOutHasValue
