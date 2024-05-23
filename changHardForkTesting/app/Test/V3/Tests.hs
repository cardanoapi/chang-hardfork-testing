{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.Tests where

import Cardano.Api qualified as C
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO)
import Hedgehog hiding (test)
import Hedgehog.Gen
import Helpers.Common (makeAddressWithStake, toShelleyBasedEra)
import Helpers.PlutusScripts (spendScriptWitness)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code qualified as PlutusTx
import Test.V3.DummyDataTypes
import Utils
import V3.Spend.VerifyBLS12G1 qualified as V3.Spend.VerifyBLS12G1
import V3.Spend.VerifyKeccak qualified as V3.Spend.VerifyKeccak
import V3.Spend.VerifySchnorr qualified as V3.Spend.VerifySchnorr

v3ScriptInfo :: NetworkId -> PlutusTx.CompiledCode a -> V3ScriptInfo
v3ScriptInfo netId compiledCode = do
    let sbs = serialiseCompiledCode compiledCode
        script = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address = makeAddressWithStake (Right hash) Nothing netId
        info = V3ScriptInfo address script hash sbs
    info

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
