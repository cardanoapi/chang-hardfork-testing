{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.VerifyBLS where

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude
import PlutusTx.Prelude qualified as P

data BLSDatum = BLSDatum
    { point1 :: BuiltinByteString
    , point2 :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''BLSDatum

-- implemented funcitons:
-- bls12_381_G1_uncompress
-- bls12_381_G1_scalarMul
-- bls12_381_G1_add
-- bls12_381_G1_equals
-- bls12_381_G1_neg
-- remaining functions:
-- bls12_381_G1_compress
-- bls12_381_G1_hashToGroup
-- bls12_381_G1_compressed_zero

{-# INLINEABLE mkValidator #-}
mkValidator :: BLSDatum -> BuiltinByteString -> ScriptContext -> Bool
mkValidator dat red ctx = (uncompress red) `bls12_381_G1_equals` datumSum
  where
    uncompress point = bls12_381_G1_uncompress point
    datumSum = (uncompress (point1 dat)) `bls12_381_G1_add` (uncompress (point2 dat))

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_) (unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [||mkWrappedValidator||])
