{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyMintingMaxExUnits where

import PlutusCore.Core (plcVersion110)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3
import PlutusTx (liftCode)
import PlutusTx qualified
import PlutusTx.Code (unsafeApplyCode)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator :: TxOutRef -> () -> ScriptContext -> Bool
mkValidator txIn _ ctx = mustMintNFT && hasInput
  where
    info = scriptContextTxInfo ctx
    mint = txInfoMint info
    inputs = map txInInfoOutRef $ txInfoInputs info
    mintedValue = flattenValue mint
    mustMintNFT = case mintedValue of
        [(_, _, n)] -> n == 1
        _ -> traceError "Must mint a single NFT"
    hasInput = txIn `elem` inputs

mkWrappedValidator :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator input red_ ctx_ = check $ mkValidator input (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: TxOutRef -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
validator input = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 input
