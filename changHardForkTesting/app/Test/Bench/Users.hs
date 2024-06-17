{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Bench.Users where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Helpers.StakePool qualified as Helper

--  generate shelley wallet
data ShelleyWallet era = ShelleyWallet
    { stakeSKey :: C.SigningKey C.StakeKey
    , paymentSKey :: C.SigningKey C.PaymentKey
    }
    deriving (Show)

generateShelleyWallet :: (MonadIO m) => m [ShelleyWallet era]
generateShelleyWallet = do
    stakeSKeys <- mapM id $ take 500 $ repeat $ liftIO $ C.generateSigningKey C.AsStakeKey
    paymentSKeys <- mapM id $ take 500 $ repeat $ liftIO $ C.generateSigningKey C.AsPaymentKey
    let stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
        wallets = zipWith (\sk pk -> ShelleyWallet sk pk) stakeSKeys paymentSKeys
    pure wallets
