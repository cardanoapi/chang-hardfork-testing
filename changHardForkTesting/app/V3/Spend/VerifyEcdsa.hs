{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyEcdsa where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude

data EcdsaComponents = EcdsaComponents
    { vk :: BuiltinByteString
    , msg :: BuiltinByteString
    , sig :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''EcdsaComponents

{-# INLINEABLE mkValidator #-}
mkValidator :: EcdsaComponents -> EcdsaComponents -> ScriptContext -> Bool
mkValidator dat red ctx =
    verifyFromDatum
        && verifyFromRedeemer
  where
    verifyFromDatum = verifyEcdsaSecp256k1Signature (vk dat) (msg dat) (sig dat)
    verifyFromRedeemer = verifyEcdsaSecp256k1Signature (vk red) (msg red) (sig red)

mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator ctx_ = check $ mkValidator (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) scriptContext
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    redeemer = getRedeemer $ scriptContextRedeemer scriptContext
    datum = case scriptContextScriptInfo scriptContext of
        SpendingScript _ dat -> case dat of
            Just dat_ -> getDatum dat_
            Nothing -> traceError "Script input has no datum"

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
