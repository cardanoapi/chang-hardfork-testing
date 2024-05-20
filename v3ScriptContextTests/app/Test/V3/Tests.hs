{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.V3.Tests where
    
import V3.VerifySchnorr qualified as V3.VerifySchnorr
import Data.Functor.Identity
import Hedgehog hiding (test)
import PlutusLedgerApi.Common (serialiseCompiledCode)
import Cardano.Api.Shelley
import TestGenerators
import Hedgehog.Gen
import Test.Gen.Cardano.Api.Typed (genTxOutValue, genValue)
import Test.V3.DummyDataTypes
import qualified Debug.Trace as Debug
import qualified Data.ByteString.Char8 as BS8
import Helpers.TestData (TestInfo(..), TestParams (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Helpers.Testnet as TN
import qualified Cardano.Api as C
import qualified Data.Time.Clock.POSIX as Time
import qualified Helpers.Query as Q
import Helpers.Common (toShelleyBasedEra, makeAddressWithStake)
import qualified Helpers.Tx as Tx
import Helpers.Test (assert)
import GHC.IO (unsafePerformIO)
import PlutusTx.IsData.Class
import Helpers.PlutusScripts (spendScriptWitness)
import qualified Cardano.Api.Shelley as C

v3ScriptInfo netId compiledCode = do 
    let sbs =  serialiseCompiledCode  compiledCode
        script =  PlutusScript PlutusScriptV3 $ PlutusScriptSerialised $ sbs
        hash = hashScript script
        address =  makeAddressWithStake (Right hash) Nothing netId
        info = V3ScriptInfo address script hash sbs
    info


verifySchnorrSignatureFromDatumAndRedeemerTestInfo =
  TestInfo
    { testName = "verifySchnorrSignatureFromDatumAndRedeemerTest"
    , testDescription =
        "Verify verifySchnorrSecp256k1Signature function for unlocking funds"
    , test = verifySchnorrSignatureFromDatumAndRedeemerTest
    }

verifySchnorrSignatureFromDatumAndRedeemerTest
  :: (MonadIO m, MonadTest m)
  => TN.TestEnvironmentOptions era
  -> TestParams era
  -> m (Maybe String)
verifySchnorrSignatureFromDatumAndRedeemerTest networkOptions TestParams{localNodeConnectInfo, pparams, networkId, tempAbsPath} = do
  era <- TN.eraFromOptionsM networkOptions
  pv <- TN.pvFromOptions networkOptions
  (w1SKey, w1Address) <- TN.w1 tempAbsPath networkId
  let sbe = toShelleyBasedEra era
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w1Address
  let 
    verifySchnorrInfo = v3ScriptInfo networkId V3.VerifySchnorr.validator
    collateral = Tx.txInsCollateral era [txIn]
    scripTxOut = 
      Tx.txOutWithInlineDatum 
      era 
      (C.lovelaceToValue 4_000_000) 
      (address verifySchnorrInfo) 
      (dataToHashableScriptData v3VerifySchnorrDatum)
    fundScriptAddress = 
      (Tx.emptyTxBodyContent sbe pparams)
        {
          C.txIns = Tx.pubkeyTxIns [txIn]
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
  (w2SKey, w2Address) <- TN.w1 tempAbsPath networkId
  txIn <- Q.adaOnlyTxInAtAddress era localNodeConnectInfo w2Address
  let 
    redeemTxOut = Tx.txOut era (C.lovelaceToValue 2_000_000) w2Address 
    scriptWitness = C.ScriptWitness C.ScriptWitnessForSpending $ 
      spendScriptWitness 
      sbe 
      (C.PlutusScriptLanguage C.PlutusScriptV3) 
      (Left $ C.PlutusScriptSerialised (sbs verifySchnorrInfo)) 
      C.InlineScriptDatum 
      (dataToHashableScriptData v3VerifySchnorrRedeemer)
    collateral = Tx.txInsCollateral era [txIn]  
    redeemFromScript = 
      (Tx.emptyTxBodyContent sbe pparams)
        {
          C.txIns = Tx.scriptTxIn [scriptTxIn] scriptWitness, 
          C.txInsCollateral = collateral,
          C.txOuts = [redeemTxOut]      
        }
  signedTx <- Tx.buildTx era localNodeConnectInfo redeemFromScript w2Address w2SKey
  Tx.submitTx sbe localNodeConnectInfo signedTx
  let redeemedTxIn = Tx.txIn (Tx.txId signedTx) 0
  resultTxOut <- Q.getTxOutAtAddress era localNodeConnectInfo w2Address redeemedTxIn "TN.getTxOutAtAddress" 
  txOutHasValue <- Q.txOutHasValue resultTxOut (C.lovelaceToValue 2_000_000)
  Helpers.Test.assert "Funds Unlocked" txOutHasValue


