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
mkValidator :: Integer -> () -> Integer -> ScriptContext -> Bool
mkValidator param dat red ctx = param == red

mkWrappedValidator :: Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator param dat_ red_ ctx_ = check $ mkValidator param (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: Integer -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator param = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 param
