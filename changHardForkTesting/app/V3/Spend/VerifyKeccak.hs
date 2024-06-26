{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyKeccak where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx (liftCode)
import PlutusTx qualified
import PlutusTx.Builtins (modInteger)
import PlutusTx.Code (unsafeApplyCode)
import PlutusTx.Prelude

data Redeem = Redeem
    { ePubKey :: BuiltinByteString
    , pkh :: PubKeyHash
    , signature :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Redeem

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinByteString -> BuiltinByteString -> Redeem -> ScriptContext -> Bool
mkValidator prefix eAddress Redeem{..} ctx =
    traceIfFalse "address verification failed" verifyAddress
        && traceIfFalse "signature verification failed" verifySignature
  where
    -- TODO: figure out how to convert PubKeyHash to txExtraKeyWits

    verifySignature = verifyEcdsaSecp256k1Signature compressedPublicKey messageHash sig
    messageHash = keccak_256 $ appendByteString prefix $ getPubKeyHash pkh
    address = sliceByteString 12 20 $ keccak_256 $ sliceByteString 1 64 ePubKey
    verifyAddress = eAddress == address
    compressedPublicKey = consByteString parityByte x
      where
        x = sliceByteString 1 32 ePubKey
        parityY = modInteger (indexByteString ePubKey 64) 2
        parityByte = if parityY == 0 then 2 else 3
    sig = sliceByteString 0 64 signature

mkWrappedValidator :: BuiltinByteString -> BuiltinData -> BuiltinUnit
mkWrappedValidator prefix ctx_ = check $ mkValidator prefix (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) scriptContext
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    redeemer = getRedeemer $ scriptContextRedeemer scriptContext
    datum = case scriptContextScriptInfo scriptContext of
        SpendingScript _ dat -> case dat of
            Just dat_ -> getDatum dat_
            Nothing -> traceError "Script input has no datum"

validator :: BuiltinByteString -> PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator prefix = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 prefix
