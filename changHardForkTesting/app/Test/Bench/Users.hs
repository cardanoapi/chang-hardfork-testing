{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bench.Users where

import Cardano.Api (NetworkId (..), NetworkMagic (NetworkMagic))
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Helpers.StakePool qualified as Helper

--  generate shelley wallet
data ShelleyWallet era = ShelleyWallet
    { stakeSKey :: C.SigningKey C.StakeKey
    , paymentSKey :: C.SigningKey C.PaymentKey
    , walletAddress :: C.Address C.ShelleyAddr
    }
    deriving (Show)

generateShelleyWallet :: (MonadIO m) => m [ShelleyWallet era]
generateShelleyWallet = do
    stakeSKeys <- mapM id $ take 150 $ repeat $ liftIO $ C.generateSigningKey C.AsStakeKey
    paymentSKeys <- mapM id $ take 150 $ repeat $ liftIO $ C.generateSigningKey C.AsPaymentKey
    let stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
        paymentCred = map (\x -> C.PaymentCredentialByKey (C.verificationKeyHash $ C.getVerificationKey x)) paymentSKeys
        walletAddress = map (\(p, s) -> C.makeShelleyAddress (Testnet $ NetworkMagic 42) p (C.StakeAddressByValue s)) (zip paymentCred stakeCreds)
        wallets = zipWith3 ShelleyWallet stakeSKeys paymentSKeys walletAddress
    pure wallets
