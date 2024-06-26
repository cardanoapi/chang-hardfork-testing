{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyBLS12G1 where

import GHC.ByteOrder (ByteOrder (LittleEndian))
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Prelude

data BLSDatum = BLSDatum
    { point1 :: BuiltinByteString
    , point2 :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BLSDatum

data BLSRedeemer = BLSRedeemer
    { addition :: BuiltinByteString
    , multiplication :: BuiltinByteString
    , negative1 :: BuiltinByteString
    , negative2 :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BLSRedeemer

-- implemented funcitons:
-- bls12_381_G1_uncompress
-- bls12_381_G1_scalarMul
-- bls12_381_G1_add
-- bls12_381_G1_equals
-- bls12_381_G1_neg
-- bls12_381_G1_compress

{-# INLINEABLE mkValidator #-}
mkValidator :: BLSDatum -> BLSRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    validateAdd
        && validateMult
        && validateNegative
  where
    validateAdd =
        redeemerSum `bls12_381_G1_equals` datumSum
    validateMult =
        redeemerProduct `bls12_381_G1_equals` datumProduct
    validateNegative =
        redeemerNegativePoint1
            `bls12_381_G1_equals` datumNegativePoint1
            && redeemerNegativePoint2
            `bls12_381_G1_equals` datumNegativePoint2

    uncompress point = bls12_381_G1_uncompress point
    compress bs = bls12_381_G1_compress bs
    redeemerSum = uncompress $ addition red
    redeemerProduct = uncompress $ multiplication red
    redeemerNegativePoint1 = uncompress $ negative1 red
    redeemerNegativePoint2 = uncompress $ negative2 red
    datumPoint1 = uncompress (point1 dat)
    datumPoint2 = uncompress (point2 dat)
    datumSum =
        datumPoint1 `bls12_381_G1_add` datumPoint2
    datumProduct =
        (byteStringToInteger (LittleEndian) (compress datumPoint1)) `bls12_381_G1_scalarMul` datumPoint2
    datumNegativePoint1 = bls12_381_G1_neg datumPoint1
    datumNegativePoint2 = bls12_381_G1_neg datumPoint2

mkWrappedValidator :: BuiltinData -> BuiltinUnit
mkWrappedValidator ctx_ = check $ mkValidator (unsafeFromBuiltinData datum) (unsafeFromBuiltinData redeemer) scriptContext
  where
    scriptContext :: ScriptContext = unsafeFromBuiltinData ctx_
    redeemer = getRedeemer $ scriptContextRedeemer scriptContext
    datum = case scriptContextScriptInfo scriptContext of
        SpendingScript _ dat -> case dat of
            Just dat_ -> getDatum dat_
            Nothing -> traceError "Script input has no datum"

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
