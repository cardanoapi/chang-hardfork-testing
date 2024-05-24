{-# LANGUAGE DataKinds #-}
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

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
