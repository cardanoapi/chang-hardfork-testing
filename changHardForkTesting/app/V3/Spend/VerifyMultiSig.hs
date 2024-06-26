{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyMultiSig where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx (liftCode)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (unsafeApplyCode)
import PlutusTx.Prelude

data MultiSigParams = MultiSigParams
    { signers :: [PubKeyHash]
    , threshold :: Integer
    }
PlutusTx.unstableMakeIsData ''MultiSigParams
PlutusTx.makeLift ''MultiSigParams

{-# INLINEABLE mkValidator #-}
mkValidator :: MultiSigParams -> [PubKeyHash] -> Bool
mkValidator params txInfoSignaories = signed
  where
    requiredSigners = threshold params
    txSigners = nub $ txInfoSignaories
    totalSigners = nub $ signers params
    activeSigners = mapMaybe (\x -> if x `elem` totalSigners then Just x else Nothing) txSigners
    signed = length activeSigners >= requiredSigners

mkWrappedValidator :: MultiSigParams -> BuiltinData -> BuiltinUnit
mkWrappedValidator msp ctx_ = check $ mkValidator msp txSigners
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    txSigners = txInfoSignatories $ scriptContextTxInfo scriptContext

validator :: MultiSigParams -> PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator params = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 params
