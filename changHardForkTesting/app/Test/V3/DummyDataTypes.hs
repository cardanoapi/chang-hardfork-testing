{-# LANGUAGE OverloadedStrings #-}

module Test.V3.DummyDataTypes where

import Cardano.Crypto.EllipticCurve.BLS12_381 qualified as BLSBindings
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BS8
import Helpers.PlutusScripts (bytesFromHex)
import Numeric (showHex)
import PlutusLedgerApi.V3 (PubKeyHash (..), toBuiltin)
import PlutusTx.Builtins
import PlutusTx.Builtins.Class qualified as BI
import V3.Spend.VerifyBLS12G1 qualified as VerifyBLS12G1
import V3.Spend.VerifyKeccak qualified as VerifyKeccak
import V3.Spend.VerifySchnorr qualified as VerifySchnorr

v3VerifySchnorrDatum :: VerifySchnorr.SchnorrComponents
v3VerifySchnorrDatum =
    VerifySchnorr.SchnorrComponents
        { VerifySchnorr.vk = BI.toBuiltin $ bytesFromHex "9f674f5d06a937e894d1cb5d0e015578890b720315395429b724a55757286dc4"
        , VerifySchnorr.msg = BI.toBuiltin $ bytesFromHex "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
        , VerifySchnorr.sig = BI.toBuiltin $ bytesFromHex "e7ab7ceb6c05cd7d5daa03bdb6cb5612abd39de910ac1ee4532d5f7072a2b35f102c1811376088d136026c5ed1608b5ecd220c81b224b79feb12e58f90f91786"
        }

v3VerifySchnorrRedeemer :: VerifySchnorr.SchnorrComponents
v3VerifySchnorrRedeemer =
    VerifySchnorr.SchnorrComponents
        { VerifySchnorr.vk = BI.toBuiltin $ bytesFromHex "407018cee8e4d35ea6c599a77b16814dbf508e6c9a424a35885a10ade3bdace8"
        , VerifySchnorr.msg = BI.toBuiltin $ bytesFromHex "c8e2c1228c0c4f143a34152c83f2ab32082d947aff989bb29b402d4a5aca25ce"
        , VerifySchnorr.sig = BI.toBuiltin $ bytesFromHex "4c4f48a975a32de295ec91f3fe2fe5c75ec257eaeb026ed10d57db84dc9204162d266133c48cba8e77c6feb951d1a38bed974849ea3e305976dc618615ec9509"
        }

v3VerifyKeccakDatum :: BuiltinByteString
v3VerifyKeccakDatum = BI.toBuiltin $ bytesFromHex "e8d3b66022a924286458484506d9bb7231d47dd9"

v3VerifyKeccakParameter :: BuiltinByteString
v3VerifyKeccakParameter = consByteString 25 "Ethereum Signed Message:\n28"

v3VerifyKeccakRedeemer :: VerifyKeccak.Redeem
v3VerifyKeccakRedeemer =
    VerifyKeccak.Redeem
        { VerifyKeccak.ePubKey = BI.toBuiltin $ bytesFromHex "04d9bdfe467bdeb307255567016e7d5e5dc08eaa80587c52c36ba68bbf7abe09e0396336748b8a8b82db653288af1578a0dd28b38bdbf9e96a5f7941d6da191efc"
        , VerifyKeccak.pkh = PubKeyHash $ BI.toBuiltin $ bytesFromHex "0cc135cf422c862735bc598d418968abf31643ba4b19c30fe22ab485"
        , VerifyKeccak.signature = BI.toBuiltin $ bytesFromHex "b32262bd655a2bc56e3a6cf9ccc5d7f49ed8d007997bb125b92e4ea30eb74c9b49fd3d18f8f72614b25802a1aa2a6b67d3e478d416da821502f1ada231cb00071c"
        }

blsG1Datum :: VerifyBLS12G1.BLSDatum
blsG1Datum =
    VerifyBLS12G1.BLSDatum
        { VerifyBLS12G1.point1 = BI.toBuiltin $ bytesFromHex "8c18eaa3c86ab258d1a24375c2189c69051a7a1aa517ba144d59840db1d38d6981a886479af77292a3b0af18a44dbf5d"
        , VerifyBLS12G1.point2 = BI.toBuiltin $ bytesFromHex "8825e51257bd8c9ff8dc605ea0c35fc00802a1b77bdcb815e720ebe8e439c2ddede9e12fd109f713f3d7b471fda5f900"
        }

blsG1Redeemer :: VerifyBLS12G1.BLSRedeemer
blsG1Redeemer =
    VerifyBLS12G1.BLSRedeemer
        { VerifyBLS12G1.addition =
            BI.toBuiltin $ bytesFromHex "ac176c975819ce26b3dfae9f4083bd5b370dd1958e9361472562ea70473b96465f7dc85faa26c91adac824c6d9b9e7a0"
        , VerifyBLS12G1.multiplication =
            BI.toBuiltin $ bytesFromHex "a5a8aee0dcac330bd6fb1d8be37894ade3e68d268c33b7b8077eddd887f0e41cac9d4874cd94492dbc323f1aa96c856f"
        , VerifyBLS12G1.negative1 =
            BI.toBuiltin $ bytesFromHex "ac18eaa3c86ab258d1a24375c2189c69051a7a1aa517ba144d59840db1d38d6981a886479af77292a3b0af18a44dbf5d"
        , VerifyBLS12G1.negative2 =
            BI.toBuiltin $ bytesFromHex "a825e51257bd8c9ff8dc605ea0c35fc00802a1b77bdcb815e720ebe8e439c2ddede9e12fd109f713f3d7b471fda5f900"
        }

-- test =
--     let point = bls12_381_G1_compress $ (VerifyBLS12G1.point1 blsG1Datum) `bls12_381_G1_hashToGroup` (VerifyBLS12G1.point2 blsG1Datum)
--     in point

-- prettyPrint :: ByteString -> String
-- prettyPrint = concat . map (flip showHex "") . BS.unpack
