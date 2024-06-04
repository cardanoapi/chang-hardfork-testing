{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.Tests where

import Cardano.Api qualified as C
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as C
import Cardano.Chain.Delegation.Validation.Interface (Environment (currentEpoch))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace qualified as Debug
import Hedgehog hiding (test)
import Hedgehog qualified as H
import Hedgehog.Gen hiding (map)
import Helpers.Common (makeAddressWithStake, toConwayEraOnwards, toShelleyBasedEra)
import Helpers.PlutusScripts (mintScriptWitness, mintScriptWitness', plutusL2, plutusL3, spendScriptWitness)
import Helpers.Query qualified as Q
import Helpers.StakePool (StakePool (..), makeStakePoolRetireCertification)
import Helpers.Staking
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters (fromCardanoTxIn, toPlutusAddress, toPlutusValue)
import Helpers.Utils qualified as U
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as O
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx.Code qualified as PlutusTx
import Test.V3.DummyDataTypes
import Utils
import V2.Mint.VerifyMintingMaxExUnits qualified
import V2.Spend.VerifyMultiSig qualified
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

v3ScriptInfo :: NetworkId -> PlutusTx.CompiledCode a -> V3ScriptInfo
v3ScriptInfo netId compiledCode = do
    let sbs = serialiseCompiledCode compiledCode
        script = PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address = makeAddressWithStake (Right hash) Nothing netId
        info = V3ScriptInfo address script hash sbs
    info

v2ScriptInfo :: NetworkId -> PlutusTx.CompiledCode a -> V2ScriptInfo
v2ScriptInfo netId compiledCode = do
    let sbs = serialiseCompiledCode compiledCode
        script = PlutusScript PlutusScriptV2 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address = makeAddressWithStake (Right hash) Nothing netId
        info = V2ScriptInfo address script hash sbs
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
            stakePool1 = stakePool !! 0
            stakePool2 = stakePool !! 1
            stakePool3 = stakePool !! 2
            regSPTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w1Address
            regSPTxBodyContent =
                (Tx.emptyTxBodyContent sbe pparams)
                    { C.txIns = Tx.pubkeyTxIns [sPRegTxIn]
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
            retireSPCert1 = makeStakePoolRetireCertification ceo stakePool1 (currentEpoch + 1)
            retireSPCert2 = makeStakePoolRetireCertification ceo stakePool2 (currentEpoch + 1)
            retireSPCert3 = makeStakePoolRetireCertification ceo stakePool3 (currentEpoch + 1)
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

verifyV3MintingEfficiencyTestInfo :: TestInfo era
verifyV3MintingEfficiencyTestInfo =
    TestInfo
        { testName = "verifyV3MintingEfficiencyTest"
        , testDescription = "Compare NFT minting transaction execution-units between `PlutusV3 with plcVersion110` and `PlutusV2 with plcVersion100` and verify V3 is efficient."
        , test = verifyV3MintingEfficiencyTest
        }

verifyV3MintingEfficiencyTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyV3MintingEfficiencyTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
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
        v3PolicyId = C.PolicyId $ v3hash verifyMaxExUnitsMintingInfoV3
        v2PolicyId = C.PolicyId $ v2hash verifyMaxExUnitsMintingInfoV2
        v3AssetId = C.AssetId v3PolicyId "MaxExUnitsMint"
        v2AssetId = C.AssetId v2PolicyId "MaxExUnitsMint"
        v3TokenValues = C.valueFromList [(v3AssetId, 1)]
        v2TokenValues = C.valueFromList [(v2AssetId, 1)]
        collateral = Tx.txInsCollateral era [txIn]
        maxExecutionUnits =
            C.ExecutionUnits
                { C.executionSteps = 10_000_000_000
                , C.executionMemory = 14_000_000
                }
        v3TxOut = Tx.txOut era (C.lovelaceToValue 4_000_000 <> v3TokenValues) w1Address
        v2TxOut = Tx.txOut era (C.lovelaceToValue 4_000_000 <> v2TokenValues) w1Address
        v3MintWitness =
            Map.fromList
                [
                    ( v3PolicyId
                    , mintScriptWitness'
                        sbe
                        plutusL3
                        (Left $ PlutusScriptSerialised (v3sbs verifyMaxExUnitsMintingInfoV3))
                        (dataToHashableScriptData ())
                        maxExecutionUnits
                    )
                ]
        v2MintWitness =
            Map.fromList
                [
                    ( v2PolicyId
                    , mintScriptWitness'
                        sbe
                        plutusL2
                        (Left $ PlutusScriptSerialised (v2sbs verifyMaxExUnitsMintingInfoV2))
                        (dataToHashableScriptData ())
                        maxExecutionUnits
                    )
                ]
        v3TxBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txMintValue = Tx.txMintValue era v3TokenValues v3MintWitness
                , C.txOuts = [v3TxOut]
                }
        v2TxBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = collateral
                , C.txMintValue = Tx.txMintValue era v2TokenValues v2MintWitness
                , C.txOuts = [v2TxOut]
                }
    v3SignedTx <- Tx.buildTx era localNodeConnectInfo v3TxBodyContent w1Address [w1SKey]
    v2SignedTx <- Tx.buildTx era localNodeConnectInfo v2TxBodyContent w1Address [w1SKey]
    (v3Mem, v3Steps) <- Tx.getTxExecutionUnits era localNodeConnectInfo (C.getTxBody v3SignedTx)
    (v2Mem, v2Steps) <- Tx.getTxExecutionUnits era localNodeConnectInfo (C.getTxBody v2SignedTx)
    let memEfficiency = ((v2Mem - v3Mem) * 100) `div` v2Mem
        stepEfficiency = ((v2Steps - v3Steps) * 100) `div` v2Steps
    Debug.traceM ("ExecutionMemory is " ++ (show memEfficiency) ++ "% efficient")
    Debug.traceM ("ExecutionSteps is " ++ (show stepEfficiency) ++ "% efficient")
    Helpers.Test.assert "V3 is efficient" (v3Mem < v2Mem && v3Steps < v2Steps)

verifyV3SpendingEfficiencyTestInfo :: TestInfo era
verifyV3SpendingEfficiencyTestInfo =
    TestInfo
        { testName = "verifyV3SpendingEfficiencyTest"
        , testDescription = "Compare redeeming from script transaction execution-units between `PlutusV3 with plcVersion110` and `PlutusV2 with plcVersion100` and verify V3 is efficient."
        , test = verifyV3SpendingEfficiencyTest
        }

verifyV3SpendingEfficiencyTest ::
    (MonadIO m, MonadTest m) =>
    TN.TestEnvironmentOptions era ->
    TestParams era ->
    m (Maybe String)
verifyV3SpendingEfficiencyTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
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
        v3parameter = V3.Spend.VerifyMultiSig.MultiSigParams pubKeyHashes threshold
        v2parameter = V2.Spend.VerifyMultiSig.MultiSigParams pubKeyHashes threshold
        v3multisigScriptInfo = v3ScriptInfo networkId (V3.Spend.VerifyMultiSig.validator v3parameter)
        v2multisigScriptInfo = v2ScriptInfo networkId (V2.Spend.VerifyMultiSig.validator v2parameter)
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
    let w1Collateral = Tx.txInsCollateral era [txIn]
        v3scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 10_000_000)
                (v3address v3multisigScriptInfo)
                (dataToHashableScriptData ())
        fundV3ScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = w1Collateral
                , C.txOuts = [v3scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundV3ScriptAddress w1Address [w1SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let v3scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    v3scriptTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v3address v3multisigScriptInfo) v3scriptTxIn "TN.getTxOutAtAddress"
    v3scriptTxOutHasValue <- Q.txOutHasValue v3scriptTxOut (C.lovelaceToValue 10_000_000)
    Helpers.Test.assert "Scripts has been funded" v3scriptTxOutHasValue
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
    let w2Collateral = Tx.txInsCollateral era [txIn]
        signers = Tx.txExtraKeyWits era [v2SKey, v3SKey]
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV3)
                    (Left $ PlutusScriptSerialised (v3sbs v3multisigScriptInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData ())
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 8_000_000) w3Address
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [v3scriptTxIn] scriptWitness
                , C.txInsCollateral = w2Collateral
                , C.txOuts = [redeemTxOut]
                , C.txExtraKeyWits = signers
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w2Address [w2SKey, w3SKey]
    (v3Mem, v3Steps) <- Tx.getTxExecutionUnits era localNodeConnectInfo (C.getTxBody signedTx)
    let v2scriptTxOut =
            Tx.txOutWithInlineDatum
                era
                (C.lovelaceToValue 10_000_000)
                (v2address v2multisigScriptInfo)
                (dataToHashableScriptData ())
        fundV2ScriptAddress =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.pubkeyTxIns [txIn]
                , C.txInsCollateral = w2Collateral
                , C.txOuts = [v2scriptTxOut]
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo fundV2ScriptAddress w2Address [w2SKey]
    Tx.submitTx sbe localNodeConnectInfo signedTx
    let v2scriptTxIn = Tx.txIn (Tx.txId signedTx) 0
    v2scriptTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo (v2address v2multisigScriptInfo) v2scriptTxIn "TN.getTxOutAtAddress"
    v2scriptTxOutHasValue <- Q.txOutHasValue v2scriptTxOut (C.lovelaceToValue 10_000_000)
    Helpers.Test.assert "Scripts has been funded" v2scriptTxOutHasValue
    txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w3Address
    let w3Collateral = Tx.txInsCollateral era [txIn]
        signers = Tx.txExtraKeyWits era [v2SKey, v3SKey]
        scriptWitness =
            C.ScriptWitness C.ScriptWitnessForSpending $
                spendScriptWitness
                    sbe
                    (C.PlutusScriptLanguage C.PlutusScriptV2)
                    (Left $ PlutusScriptSerialised (v2sbs v2multisigScriptInfo))
                    C.InlineScriptDatum
                    (dataToHashableScriptData ())
        redeemTxOut = Tx.txOut era (C.lovelaceToValue 8_000_000) w1Address
        txBodyContent =
            (Tx.emptyTxBodyContent sbe pparams)
                { C.txIns = Tx.scriptTxIn [v2scriptTxIn] scriptWitness
                , C.txInsCollateral = w3Collateral
                , C.txOuts = [redeemTxOut]
                , C.txExtraKeyWits = signers
                }
    signedTx <- Tx.buildTx era localNodeConnectInfo txBodyContent w3Address [w2SKey, w3SKey]
    (v2Mem, v2Steps) <- Tx.getTxExecutionUnits era localNodeConnectInfo (C.getTxBody signedTx)
    let memEfficiency = ((v2Mem - v3Mem) * 100) `div` v2Mem
        stepEfficiency = ((v2Steps - v3Steps) * 100) `div` v2Steps
    Debug.traceM ("ExecutionMemory is " ++ (show memEfficiency) ++ "% efficient")
    Debug.traceM ("ExecutionSteps is " ++ (show stepEfficiency) ++ "% efficient")
    Helpers.Test.assert "V3 is efficient" (v3Mem < v2Mem && v3Steps < v2Steps)
