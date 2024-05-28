{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.Tests where

import Cardano.Api qualified as C
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import Debug.Trace qualified as Debug
import Hedgehog hiding (test)
import Hedgehog.Gen hiding (map)
import Helpers.Common (makeAddressWithStake, toShelleyBasedEra)
import Helpers.PlutusScripts (mintScriptWitness, mintScriptWitness', plutusL3, spendScriptWitness)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters (fromCardanoTxIn, toPlutusAddress, toPlutusValue)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code qualified as PlutusTx
import Test.V3.DummyDataTypes
import Utils
import V3.Mint.VerifyMintingMaxExUnits qualified as V3.Mint.VerifyMintingMaxExUnits
import V3.Spend.SimpleScript qualified as V3.Spend.SimpleScript
import V3.Spend.VerifyBLS12G1 qualified as V3.Spend.VerifyBLS12G1
import V3.Spend.VerifyBLS12G2 qualified as V3.Spend.VerifyBLS12G2
import V3.Spend.VerifyBlake2b224 qualified as V3.Spend.VerifyBlake2b224
import V3.Spend.VerifyEcdsa qualified as V3.Spend.VerifyEcdsa
import V3.Spend.VerifyEd25519 qualified as V3.Spend.VerifyEd25519
import V3.Spend.VerifyKeccak qualified as V3.Spend.VerifyKeccak
import V3.Spend.VerifyRefInputVisibility qualified as V3.Spend.VerifyRefInputVisibility
import V3.Spend.VerifySchnorr qualified as V3.Spend.VerifySchnorr

v3ScriptInfo :: NetworkId -> PlutusTx.CompiledCode a -> V3ScriptInfo
v3ScriptInfo netId compiledCode = do
    let sbs = serialiseCompiledCode compiledCode
        script = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address = makeAddressWithStake (Right hash) Nothing netId
        info = V3ScriptInfo address script hash sbs
    info

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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyEcdsaInfo = v3ScriptInfo networkId V3.Spend.VerifyEcdsa.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyEcdsaInfo)
                (dataToHashableScriptData v3VerifyEcdsaDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyEcdsaInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (sbs verifyEcdsaInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyEcdsaRedeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyEd25519Info = v3ScriptInfo networkId V3.Spend.VerifyEd25519.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyEd25519Info)
                (dataToHashableScriptData v3VerifyEd25519Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyEd25519Info) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (sbs verifyEd25519Info))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyEd25519Datum)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifySchnorrInfo = v3ScriptInfo networkId V3.Spend.VerifySchnorr.validator
        collateral = Tx.txInsCollateral era [txIn]
        scripTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifySchnorrInfo)
                (dataToHashableScriptData v3VerifySchnorrDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scripTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifySchnorrInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ C.PlutusScriptSerialised (sbs verifySchnorrInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifySchnorrRedeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyKeccakInfo = v3ScriptInfo networkId (V3.Spend.VerifyKeccak.validator v3VerifyKeccakParameter)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyKeccakInfo)
                (dataToHashableScriptData v3VerifyKeccakDatum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyKeccakInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (sbs verifyKeccakInfo))
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
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBLSInfo = v3ScriptInfo networkId (V3.Spend.VerifyBLS12G1.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyBLSInfo)
                (dataToHashableScriptData blsG1Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyBLSInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (sbs verifyBLSInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData blsG1Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBLSInfo = v3ScriptInfo networkId (V3.Spend.VerifyBLS12G2.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyBLSInfo)
                (dataToHashableScriptData blsG2Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyBLSInfo) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (sbs verifyBLSInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData blsG2Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
    let sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let
        verifyBlake2b224Info = v3ScriptInfo networkId (V3.Spend.VerifyBlake2b224.validator)
        collateral = Tx.txInsCollateral era [txIn]
        scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address verifyBlake2b224Info)
                (dataToHashableScriptData v3VerifyBlake2b224Datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyBlake2b224Info) scriptTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 4_000_000)
    Helpers.Test.assert "Script has been funded" txOutHasValue
    -- redeeming from script
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 3_000_000) w2Address
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (sbs verifyBlake2b224Info))
                    C.InlineScriptDatum
                    (dataToHashableScriptData v3VerifyBlake2b224Redeemer)
        collateral = Tx.txInsCollateral era [txIn]
        redeemFromScript =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress"
    txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 3_000_000)
    Helpers.Test.assert "Funds Unlocked" txOutHasValue

verifyReferenceInputVisibilityTestInfo :: TestInfo era
verifyReferenceInputVisibilityTestInfo =
    TestInfo
        { testName = "verifyReferenceInputVisibilityTestInfo"
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
        (w2SKey, w2Address) = skeyAndAddress !! 1
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
    signedTx <- Tx.buildTx era localNodeConnectInfo fundRefInput w1Address w1SKey
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
                (address verifyRefInputVisibilityInfo)
                (dataToHashableScriptData datum)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (address verifyRefInputVisibilityInfo) scriptTxIn "TN.getTxOutAtAddress"
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
                    (Left $ PlutusScriptSerialised (sbs verifyRefInputVisibilityInfo))
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
    signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w1Address w1SKey
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
    let (w1SKey, w1Address) = skeyAndAddress !! 0
        sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    txInAsTxOut@(C.TxOut _ txInValue _ _) <-
        Q.getTxOutAtAddress era localNodeConnectInfo w1Address txIn "txInAsTxOut <- getTxOutAtAddress"
    let scriptValidator = V3.Mint.VerifyMintingMaxExUnits.validator (fromCardanoTxIn txIn)
        verifyMaxExUnitsMintingInfo = v3ScriptInfo networkId scriptValidator
        policyId = C.PolicyId $ hash verifyMaxExUnitsMintingInfo
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
                        (Left $ PlutusScriptSerialised (sbs verifyMaxExUnitsMintingInfo))
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
    Helpers.Test.assert "Tokens Minted" txOutHasValue

-- locking multiple UTxOs in the same script address in the same transaction
-- spending multiple UTxOs from the same sctipt address in the same transaction
verifyLockingAndSpendingInSameTxTestInfo :: TestInfo era
verifyLockingAndSpendingInSameTxTestInfo =
    TestInfo
        { testName = "verifyLockingAndSpendingInSameTxTest"
        , testDescription =
            "Verify locking and spending multiple UTxOs in/from the same script address in the same transaction."
        , test = verifyLockingAndSpendingInSameTxTest
        }

verifyLockingAndSpendingInSameTxTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyLockingAndSpendingInSameTxTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
    era <- TN.eraFromOptionsM networkOptions
    skeyAndAddress <- TN.w tempAbsPath networkId
    let (w1SKey, w1Address) = skeyAndAddress !! 0
        sbe = toShelleyBasedEra era
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let collateral = Tx.txInsCollateral era [txIn]
        datumRedeemer :: Integer = 10046737
        simpleScriptInfo = v3ScriptInfo networkId (V3.Spend.SimpleScript.validator)
        scriptTxOut1 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 4_000_000)
                (address simpleScriptInfo)
                (dataToHashableScriptData datumRedeemer)
        scriptTxOut2 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 3_000_000)
                (address simpleScriptInfo)
                (dataToHashableScriptData datumRedeemer)
        scriptTxOut3 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 2_000_000)
                (address simpleScriptInfo)
                (dataToHashableScriptData datumRedeemer)
        fundScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txOuts = [scriptTxOut1, scriptTxOut2, scriptTxOut3]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundScriptAddress w1Address w1SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let scriptTxIn1 = Tx.txIn (Tx.txId signedTx) 0
        scriptTxIn2 = Tx.txIn (Tx.txId signedTx) 1
        scriptTxIn3 = Tx.txIn (Tx.txId signedTx) 2
    resultTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo (address simpleScriptInfo) scriptTxIn1 "TN.getTxOutAtAddress"
    txOutHasValue1 <- Q.txOutHasValue resultTxOut1 (C.lovelaceToValue 4_000_000)
    resultTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo (address simpleScriptInfo) scriptTxIn2 "TN.getTxOutAtAddress"
    txOutHasValue2 <- Q.txOutHasValue resultTxOut2 (C.lovelaceToValue 3_000_000)
    resultTxOut3 <- Q.getTxOutAtAddress era localNodeConnectInfo (address simpleScriptInfo) scriptTxIn3 "TN.getTxOutAtAddress"
    txOutHasValue3 <- Q.txOutHasValue resultTxOut3 (C.lovelaceToValue 2_000_000)
    Helpers.Test.assert "Script has been funded" (txOutHasValue1 && txOutHasValue2 && txOutHasValue3)
    -- redeem 2 UtxOs from script and fund 2 UtxOs to sctipt in the same transaction
    let (w2SKey, w2Address) = skeyAndAddress !! 1
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let collateral = Tx.txInsCollateral era [txIn]
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (sbs simpleScriptInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData datumRedeemer)
        redeemTxOut1 = Tx.txOut era (C.lovelaceToValue 4_000_000) w2Address
        redeemTxOut2 = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address
        scriptTxOut4 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 5_000_000)
                (address simpleScriptInfo)
                (dataToHashableScriptData datumRedeemer)
        scriptTxOut5 =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 6_000_000)
                (address simpleScriptInfo)
                (dataToHashableScriptData datumRedeemer)
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns =
                    (Tx.scriptTxIn [scriptTxIn1, scriptTxIn2] scriptWitness)
                        ++ (Tx.pubkeyTxIns [txIn])
                , C.txInsCollateral = collateral
                , C.txOuts = [redeemTxOut1, redeemTxOut2, scriptTxOut4, scriptTxOut5]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Address w2SKey
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let redeemedTxIn1 = Tx.txIn (Tx.txId signedTx) 0
        redeemedTxIn2 = Tx.txIn (Tx.txId signedTx) 1
        scriptTxIn4 = Tx.txIn (Tx.txId signedTx) 2
        scriptTxIn5 = Tx.txIn (Tx.txId signedTx) 3
    walletTxOut1 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn1 "TN.getTxOutAtAddress"
    walletTxOut2 <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn2 "TN.getTxOutAtAddress"
    scriptTxOut4 <- Q.getTxOutAtAddress era localNodeConnectInfo (address simpleScriptInfo) scriptTxIn4 "TN.getTxOutAtAddress"
    scriptTxOut5 <- Q.getTxOutAtAddress era localNodeConnectInfo (address simpleScriptInfo) scriptTxIn5 "TN.getTxOutAtAddress"
    walletTxOutHasValue1 <- Q.txOutHasValue walletTxOut1 (C.lovelaceToValue 4_000_000)
    walletTxOutHasValue2 <- Q.txOutHasValue walletTxOut2 (C.lovelaceToValue 2_000_000)
    scriptTxOutHasValue4 <- Q.txOutHasValue scriptTxOut4 (C.lovelaceToValue 5_000_000)
    scriptTxOutHasValue5 <- Q.txOutHasValue scriptTxOut5 (C.lovelaceToValue 6_000_000)
    Helpers.Test.assert "Funds Locked and Unlocked" (walletTxOutHasValue1 && walletTxOutHasValue2 && scriptTxOutHasValue4 && scriptTxOutHasValue5)
