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
import PlutusLedgerApi.V1.Bytes qualified as P
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
