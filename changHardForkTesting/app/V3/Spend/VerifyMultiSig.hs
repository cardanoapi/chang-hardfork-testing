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
import PlutusTx.Code (unsafeApplyCode)
import PlutusTx.Prelude

data MultiSigParams = MultiSigParams
    { signers :: [PubKeyHash]
    , threshold :: Integer
    }
PlutusTx.unstableMakeIsData ''MultiSigParams
PlutusTx.makeLift ''MultiSigParams

{-# INLINEABLE mkValidator #-}
mkValidator :: MultiSigParams -> () -> () -> ScriptContext -> Bool
mkValidator params dat red ctx = signed
  where
    info = scriptContextTxInfo ctx
    requiredSigners = threshold params
    txSigners = nub $ txInfoSignatories info
    totalSigners = nub $ signers params
    activeSigners = mapMaybe (\x -> if x `elem` totalSigners then Just x else Nothing) txSigners
    signed = length activeSigners >= requiredSigners

mkWrappedValidator :: MultiSigParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator msp dat_ red_ ctx_ = check $ mkValidator msp (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: MultiSigParams -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator params = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 params