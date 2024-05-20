{-#LANGUAGE NoImplicitPrelude#-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

module V3.VerifySchnorr where 

import PlutusTx.Prelude 
import PlutusLedgerApi.V3
import qualified PlutusTx

data SchnorrComponents = SchnorrComponents {
    vk :: BuiltinByteString,
    msg:: BuiltinByteString,
    sig :: BuiltinByteString
}
PlutusTx.makeIsDataIndexed ''SchnorrComponents [('SchnorrComponents, 0)]

{-# INLINABLE mkValidator #-}
mkValidator:: SchnorrComponents-> SchnorrComponents -> ScriptContext -> Bool 
mkValidator dat red ctx = verifyFromDatum && verifyFromRedeemer
    where 
        verifyFromDatum = verifySchnorrSecp256k1Signature (vk dat) (msg dat) (sig dat) 
        verifyFromRedeemer = verifySchnorrSecp256k1Signature (vk red) (msg red) (sig red) 

mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator dat_ red_ ctx_ = check $ mkValidator (unsafeFromBuiltinData dat_)(unsafeFromBuiltinData red_) (unsafeFromBuiltinData ctx_)

validator :: PlutusTx.CompiledCode(BuiltinData -> BuiltinData -> BuiltinData -> ())
validator = $$(PlutusTx.compile [|| mkWrappedValidator ||])