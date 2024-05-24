{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyEd25519 where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude

data Ed25519Components = Ed25519Components
    { vk :: BuiltinByteString
    , msg :: BuiltinByteString
    , sig :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Ed25519Components

{-# INLINEABLE mkValidator #-}
mkValidator :: Ed25519Components -> Ed25519Components -> ScriptContext -> Bool
mkValidator dat red ctx =
    verifyFromDatum
        && verifyFromRedeemer
  where
    verifyFromDatum = verifyEd25519Signature (vk dat) (msg dat) (sig dat)
    verifyFromRedeemer = verifyEd25519Signature (vk red) (msg red) (sig red)

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
