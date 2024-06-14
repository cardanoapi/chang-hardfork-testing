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
    , stakeCred :: C.StakeCredential
    }
    deriving (Show)

generateShelleyWallet :: (MonadIO m) => m [ShelleyWallet era]
generateShelleyWallet = do
    stakeSKeys <- mapM id $ take 100 $ repeat $ liftIO $ C.generateSigningKey C.AsStakeKey
    let stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
        wallets = zipWith (\sk cred -> ShelleyWallet sk cred) stakeSKeys stakeCreds
    pure wallets

-- 100 shelley wallets generated
-- 20 DReps generated
-- 4 CC members
