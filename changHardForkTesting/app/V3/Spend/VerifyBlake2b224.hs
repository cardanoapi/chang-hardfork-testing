{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyBlake2b224 where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins (equalsByteString)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
-- pubKeyHash -> pubKey -> context -> bool
mkValidator :: BuiltinByteString -> BuiltinByteString -> ScriptContext -> Bool
mkValidator dat red ctx = dat `equalsByteString` (blake2b_224 red)

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
