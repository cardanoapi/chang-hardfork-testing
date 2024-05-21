{-# LANGUAGE OverloadedStrings #-}

module Test.V3.DummyDataTypes where

import Data.ByteString.Char8 qualified as BS8
import Helpers.PlutusScripts (bytesFromHex)
import PlutusLedgerApi.V3 (PubKeyHash (..), toBuiltin)
import PlutusTx.Builtins
import PlutusTx.Builtins.Class qualified as BI
import V3.VerifyKeccak
import V3.VerifySchnorr (SchnorrComponents (..))

v3VerifySchnorrDatum :: SchnorrComponents
v3VerifySchnorrDatum =
    SchnorrComponents
        { vk = BI.toBuiltin $ bytesFromHex "9f674f5d06a937e894d1cb5d0e015578890b720315395429b724a55757286dc4"
        , msg = BI.toBuiltin $ bytesFromHex "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
        , sig = BI.toBuiltin $ bytesFromHex "e7ab7ceb6c05cd7d5daa03bdb6cb5612abd39de910ac1ee4532d5f7072a2b35f102c1811376088d136026c5ed1608b5ecd220c81b224b79feb12e58f90f91786"
        }

v3VerifySchnorrRedeemer :: SchnorrComponents
v3VerifySchnorrRedeemer =
    SchnorrComponents
        { vk = BI.toBuiltin $ bytesFromHex "407018cee8e4d35ea6c599a77b16814dbf508e6c9a424a35885a10ade3bdace8"
        , msg = BI.toBuiltin $ bytesFromHex "c8e2c1228c0c4f143a34152c83f2ab32082d947aff989bb29b402d4a5aca25ce"
        , sig = BI.toBuiltin $ bytesFromHex "4c4f48a975a32de295ec91f3fe2fe5c75ec257eaeb026ed10d57db84dc9204162d266133c48cba8e77c6feb951d1a38bed974849ea3e305976dc618615ec9509"
        }

v3VerifyKeccakDatum :: BuiltinByteString
v3VerifyKeccakDatum = BI.toBuiltin $ bytesFromHex "e8d3b66022a924286458484506d9bb7231d47dd9"

v3VerifyKeccakParameter :: BuiltinByteString
v3VerifyKeccakParameter = consByteString 25 "Ethereum Signed Message:\n28"

v3VerifyKeccakRedeemer :: Redeem
v3VerifyKeccakRedeemer =
    Redeem
        { ePubKey = BI.toBuiltin $ bytesFromHex "04d9bdfe467bdeb307255567016e7d5e5dc08eaa80587c52c36ba68bbf7abe09e0396336748b8a8b82db653288af1578a0dd28b38bdbf9e96a5f7941d6da191efc"
        , pkh = PubKeyHash $ BI.toBuiltin $ bytesFromHex "0cc135cf422c862735bc598d418968abf31643ba4b19c30fe22ab485"
        , signature = BI.toBuiltin $ bytesFromHex "b32262bd655a2bc56e3a6cf9ccc5d7f49ed8d007997bb125b92e4ea30eb74c9b49fd3d18f8f72614b25802a1aa2a6b67d3e478d416da821502f1ada231cb00071c"
        }
