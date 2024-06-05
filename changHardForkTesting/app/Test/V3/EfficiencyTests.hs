{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.V3.EfficiencyTests where

import Cardano.Api qualified as C
import Cardano.Api.Shelley
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace qualified as Debug
import Hedgehog hiding (test)
import Helpers.Common (toShelleyBasedEra)
import Helpers.PlutusScripts (mintScriptWitness', plutusL2, plutusL3, spendScriptWitness)
import Helpers.Query qualified as Q
import Helpers.Test (assert)
import Helpers.TestData (TestInfo (..), TestParams (..))
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import Helpers.TypeConverters (fromCardanoTxIn)
import Utils
import V2.Mint.VerifyMintingMaxExUnits qualified
import V2.Spend.VerifyMultiSig qualified
import V3.Mint.VerifyMintingMaxExUnits qualified
import V3.Spend.VerifyMultiSig qualified

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
