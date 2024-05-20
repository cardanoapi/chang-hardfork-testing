{-#LANGUAGE OverloadedStrings #-}
module Test.V3.DummyDataTypes where 

import V3.VerifySchnorr (SchnorrComponents(..)) 
import qualified Data.ByteString.Char8 as BS8
import PlutusLedgerApi.V3 (toBuiltin)
import Helpers.PlutusScripts (bytesFromHex)
import qualified PlutusTx.Builtins.Class as BI

v3VerifySchnorrDatum :: SchnorrComponents
v3VerifySchnorrDatum = SchnorrComponents {
    vk = BI.toBuiltin $ bytesFromHex "9f674f5d06a937e894d1cb5d0e015578890b720315395429b724a55757286dc4",
    msg =BI.toBuiltin $ bytesFromHex "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
    sig = BI.toBuiltin $ bytesFromHex"e7ab7ceb6c05cd7d5daa03bdb6cb5612abd39de910ac1ee4532d5f7072a2b35f102c1811376088d136026c5ed1608b5ecd220c81b224b79feb12e58f90f91786"
}

v3VerifySchnorrRedeemer :: SchnorrComponents
v3VerifySchnorrRedeemer = SchnorrComponents {
    vk =  BI.toBuiltin $ bytesFromHex"407018cee8e4d35ea6c599a77b16814dbf508e6c9a424a35885a10ade3bdace8",
    msg =  BI.toBuiltin $ bytesFromHex"c8e2c1228c0c4f143a34152c83f2ab32082d947aff989bb29b402d4a5aca25ce",
    sig =  BI.toBuiltin $ bytesFromHex"4c4f48a975a32de295ec91f3fe2fe5c75ec257eaeb026ed10d57db84dc9204162d266133c48cba8e77c6feb951d1a38bed974849ea3e305976dc618615ec9509"
}
