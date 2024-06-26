{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Mint.VerifyMintingMaxExUnits where

import PlutusCore.Core (plcVersion110)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3
import PlutusTx (liftCode)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Code (unsafeApplyCode)
import PlutusTx.Prelude

{-# INLINEABLE mkValidator #-}
mkValidator :: TxOutRef -> [TxInInfo] -> Value -> Bool
mkValidator txIn inputsInfo mint = mustMintNFT && hasInput
  where
    inputs = map txInInfoOutRef $ inputsInfo
    mintedValue = flattenValue mint
    mustMintNFT = case mintedValue of
        [(_, _, n)] -> n == 1
        _ -> traceError "Must mint a single NFT"
    hasInput = txIn `elem` inputs

mkWrappedValidator :: TxOutRef -> BuiltinData -> BuiltinUnit
mkWrappedValidator input ctx_ = check $ mkValidator input (unsafeFromBuiltinData txInputs) (unsafeFromBuiltinData txMint)
  where
    ds :: BuiltinData -> BI.BuiltinList BuiltinData
    ds bd = BI.snd (BI.unsafeDataAsConstr bd)

    context = ds ctx_

    txInfo = ds $ BI.head context

    txInputs = BI.head txInfo

    txMint = BI.head $ BI.tail $ BI.tail $ BI.tail $ BI.tail txInfo

validator :: TxOutRef -> PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator input = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 input
