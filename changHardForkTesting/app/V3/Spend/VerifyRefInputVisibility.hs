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
mkValidator :: RefInputConfig -> () -> ScriptContext -> Bool
mkValidator dat red ctx = verifyDatumVisible && verifyValueVisible && verifyAddressVisible
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

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
