{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.SimpleScript where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins (equalsByteString)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator dat red ctx = dat == red

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
