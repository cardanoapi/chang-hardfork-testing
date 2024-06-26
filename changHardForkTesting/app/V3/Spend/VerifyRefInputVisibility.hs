{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyRefInputVisibility where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude

data RefInputConfig = RefInputConfig
    { address :: Address
    , value :: Value
    , datum :: Integer
    }
PlutusTx.unstableMakeIsData ''RefInputConfig

{-# INLINEABLE expectSingle #-}
expectSingle :: [a] -> a
expectSingle list = case list of
    [a] -> a
    _ -> traceError "expected single element in list"

{-# INLINEABLE mkValidator #-}
mkValidator :: RefInputConfig -> ScriptContext -> Bool
mkValidator dat ctx = verifyDatumVisible && verifyValueVisible && verifyAddressVisible
  where
    info = scriptContextTxInfo ctx
    referenceInput = txInInfoResolved $ expectSingle $ txInfoReferenceInputs info
    refInputAddress = txOutAddress referenceInput
    refInputValue = txOutValue referenceInput
    refInputDatum = case txOutDatum referenceInput of
        NoOutputDatum -> traceError "Reference Input Has No Datum"
        OutputDatumHash dh -> traceError "Reference Input Has Datum Hash"
        OutputDatum da -> case fromBuiltinData (getDatum da) of
            Just any -> any
            Nothing -> traceError "Reference Input Datum Invalid"
    verifyDatumVisible = refInputDatum == (datum dat)
    verifyValueVisible = refInputValue == (value dat)
    verifyAddressVisible = refInputAddress == (address dat)

mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator ctx_ = check $ mkValidator (unsafeFromBuiltinData datum) scriptContext
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    datum = case scriptContextScriptInfo scriptContext of
        SpendingScript _ dat -> case dat of
            Just dat -> getDatum dat
            Nothing -> traceError "Script input has no datum"

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
