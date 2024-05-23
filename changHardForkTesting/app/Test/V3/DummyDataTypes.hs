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
import V3.Spend.VerifyBLS12G2 qualified as VerifyBLS12G2
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

blsG2Datum :: VerifyBLS12G2.BLSDatum
blsG2Datum =
    VerifyBLS12G2.BLSDatum
        { VerifyBLS12G2.point1 =
            BI.toBuiltin $
                bytesFromHex $
                    "b05f1802c3bb555b3c88dda75b94eac37a26421009376ebc80dd7a2d89deb772062ee3aed305fa238e9f308993e4f878"
                        <> "00db326658ab24b7a238438b822475df529057792ef00924cc3a546a323deac55907594ed8696fc9dd1fb3c38be4a7b8"
        , VerifyBLS12G2.point2 =
            BI.toBuiltin $
                bytesFromHex $
                    "8dee544f788c5a1c1f24c06e9ebace025563bd59aff47d40031f8dba5b555f5b6d1376895d732a357131c9c541b6db02"
                        <> "181de8d393c5b5a7ce82ad0d050bfab1d9b1006043c79c08ed99b131ba7bbe84e960ec4ab0bf4b919ebeebc0a07a9821"
        }

blsG2Redeemer :: VerifyBLS12G2.BLSRedeemer
blsG2Redeemer =
    VerifyBLS12G2.BLSRedeemer
        { VerifyBLS12G2.addition =
            BI.toBuiltin $
                bytesFromHex $
                    "94a318acbb010d52b0a6918b60dcd72a305290b2c9376a24ad7f15518bcdddec018411b509fdb753571832fc4aa40dc71"
                        <> "020013cd9fd0d461cf5dcca7478c0787cb286bcc60cfe3ee375a56c2e3baf6fc4ac9ab32cd3eb95194f5a4ac587c23d"
        , VerifyBLS12G2.multiplication =
            BI.toBuiltin $
                bytesFromHex $
                    "abc46d96c317f25e2b689de614e65f0bc298e14d525898c8bc04ee2dd7835d8621ee38d82d4c89e68b5303ef9029cba30"
                        <> "aa7369b495b6467a204d6238ec907367eaa21cfef8ad79d5ac69b949238c00c93c0453968026c948a506a897c36ee22"
        , VerifyBLS12G2.negative1 =
            BI.toBuiltin $
                bytesFromHex $
                    "905f1802c3bb555b3c88dda75b94eac37a26421009376ebc80dd7a2d89deb772062ee3aed305fa238e9f308993e4f8780"
                        <> "0db326658ab24b7a238438b822475df529057792ef00924cc3a546a323deac55907594ed8696fc9dd1fb3c38be4a7b8"
        , VerifyBLS12G2.negative2 =
            BI.toBuiltin $
                bytesFromHex $
                    "adee544f788c5a1c1f24c06e9ebace025563bd59aff47d40031f8dba5b555f5b6d1376895d732a357131c9c541b6db021"
                        <> "81de8d393c5b5a7ce82ad0d050bfab1d9b1006043c79c08ed99b131ba7bbe84e960ec4ab0bf4b919ebeebc0a07a9821"
        }

-- test =
--     let point = bls12_381_G2_compress $ (bls12_381_G2_uncompress $ VerifyBLS12G2.point1 blsG2Datum) `bls12_381_G2_add` (bls12_381_G2_uncompress $ VerifyBLS12G2.point2 blsG2Datum)
--     in BI.fromBuiltin $ point

-- prettyPrint :: ByteString -> String
-- prettyPrint = concat . map (flip showHex "") . BS.unpack
