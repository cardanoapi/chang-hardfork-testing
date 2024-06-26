{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
-- smart contract 1 liner
-- new functionality of plutus v3
{-# LANGUAGE NumericUnderscores #-}

module Utils where

import Cardano.Api
import Cardano.Api qualified as C
import Cardano.Api.Shelley hiding (Mainnet)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.Kind (Type)
import Debug.Trace qualified as Debug
import Helpers.Common (makeAddressWithStake)
import PlutusLedgerApi.Common
import PlutusLedgerApi.V1.Bytes qualified as P
import PlutusTx qualified
import PlutusTx.IsData.Class

data V3ScriptInfo = V3ScriptInfo
    { v3address :: Address ShelleyAddr
    , v3script :: Script PlutusScriptV3
    , v3hash :: ScriptHash
    , v3sbs :: ShortByteString
    }

data V2ScriptInfo = V2ScriptInfo
    { v2address :: Address ShelleyAddr
    , v2script :: Script PlutusScriptV2
    , v2hash :: ScriptHash
    , v2sbs :: ShortByteString
    }

data FunderInfo era = FunderInfo
    { input :: C.TxIn
    , collateral :: C.TxInsCollateral era
    , value :: C.Value
    , funderAddress :: Address ShelleyAddr
    , sKey :: SigningKey PaymentKey
    }

dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData = fromPlutusData $ toData sData

dataToHashableScriptData :: (ToData a1) => a1 -> HashableScriptData
dataToHashableScriptData sData = unsafeHashableScriptData (dataToScriptData sData)

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e) = error $ show e
    fromEither (Right b) = b

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

consoleLog :: forall (f :: Type -> Type). (Applicative f) => String -> f ()
consoleLog str = Debug.traceM (str)
