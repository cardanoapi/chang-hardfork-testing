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
mkValidator :: BuiltinByteString -> BuiltinByteString -> Bool
mkValidator dat red = dat `equalsByteString` (blake2b_224 red)

mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator ctx_ = check $ mkValidator (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer)
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    redeemer = getRedeemer $ scriptContextRedeemer scriptContext
    datum = case scriptContextScriptInfo scriptContext of
        SpendingScript _ dat -> case dat of
            Just dat_ -> getDatum dat_
            Nothing -> traceError "Script input has no datum"

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
