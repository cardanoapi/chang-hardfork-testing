{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module V2.Spend.VerifyMultiSig where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V2
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
mkValidator :: MultiSigParams -> () -> () -> [PubKeyHash] -> Bool
mkValidator params dat red txInfoSignaories = signed
  where
    requiredSigners = threshold params
    txSigners = nub $ txInfoSignaories
    totalSigners = nub $ signers params
    activeSigners = mapMaybe (\x -> if x `elem` totalSigners then Just x else Nothing) txSigners
    signed = length activeSigners >= requiredSigners

mkWrappedValidator :: MultiSigParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator msp dat_ red_ ctx_ = check $ mkValidator msp (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData txSigners)
  where
    ds :: BuiltinData -> BI.BuiltinList BuiltinData
    ds bd = BI.snd (BI.unsafeDataAsConstr bd)

    context = ds ctx_

    txInfo = BI.head context

    txSigners =
        BI.head
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ ds txInfo

validator :: MultiSigParams -> PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator params = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion100 params
