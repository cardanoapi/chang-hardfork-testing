{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyBLS12G2 where

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

{-# INLINEABLE mkValidator #-}
mkValidator :: BLSDatum -> BLSRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    validateAdd
        && validateMult
        && validateNegative
  where
    validateAdd =
        redeemerSum `bls12_381_G2_equals` datumSum
    validateMult =
        redeemerProduct `bls12_381_G2_equals` datumProduct
    validateNegative =
        redeemerNegativePoint1
            `bls12_381_G2_equals` datumNegativePoint1
            && redeemerNegativePoint2
            `bls12_381_G2_equals` datumNegativePoint2

    uncompress point = bls12_381_G2_uncompress point
    compress bs = bls12_381_G2_compress bs
    redeemerSum = uncompress $ addition red
    redeemerProduct = uncompress $ multiplication red
    redeemerNegativePoint1 = uncompress $ negative1 red
    redeemerNegativePoint2 = uncompress $ negative2 red
    datumPoint1 = uncompress (point1 dat)
    datumPoint2 = uncompress (point2 dat)
    datumSum =
        datumPoint1 `bls12_381_G2_add` datumPoint2
    datumProduct =
        (byteStringToInteger (LittleEndian) (compress datumPoint1)) `bls12_381_G2_scalarMul` datumPoint2
    datumNegativePoint1 = bls12_381_G2_neg datumPoint1
    datumNegativePoint2 = bls12_381_G2_neg datumPoint2

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
