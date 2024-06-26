{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.SimpleScript where

import PlutusCore.Core (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx (unsafeApplyCode)
import PlutusTx qualified
import PlutusTx.Lift (liftCode)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> Integer -> Bool
mkValidator param red = param == red

mkWrappedValidator :: Integer -> BuiltinData -> BuiltinUnit
mkWrappedValidator param ctx_ = check $ mkValidator param (unsafeFromBuiltinData redeemer)
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    redeemer = getRedeemer $ scriptContextRedeemer scriptContext

validator :: Integer -> PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator param = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 param
