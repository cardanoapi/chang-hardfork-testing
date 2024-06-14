{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Staking where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (zipWith4)
import Helpers.StakePool qualified as Helper

data Staking era = Staking
    { stakeSKey :: C.SigningKey C.StakeKey
    , stakeCred :: C.StakeCredential
    , stakeRegCert :: C.Certificate era
    , stakeUnregCert :: C.Certificate era
    , stakeDelegationPool :: Helper.StakePool era -- pool to delegate to
    }
    deriving (Show)

generateStakeKeyCredentialAndCertificate ::
    (MonadIO m) =>
    C.ConwayEraOnwards era ->
    Helper.StakePool era ->
    m [(Staking era)]
generateStakeKeyCredentialAndCertificate ceo stakePool = do
    stakeSKeys <- mapM id $ take 3 $ repeat $ liftIO $ C.generateSigningKey C.AsStakeKey
    let
        stakeCreds = map (\x -> C.StakeCredentialByKey $ C.verificationKeyHash $ C.getVerificationKey x) stakeSKeys
        stakeDeposit = C.Lovelace 0 -- keyDeposit
        stakeReqs = map (\x -> C.StakeAddrRegistrationConway ceo stakeDeposit x) stakeCreds
        stakeRegCerts = map C.makeStakeAddressRegistrationCertificate stakeReqs
        stakeUnregCerts = map C.makeStakeAddressUnregistrationCertificate stakeReqs
        stakingList = zipWith4 (\sk cred regCert unregCert -> Staking sk cred regCert unregCert stakePool) stakeSKeys stakeCreds stakeRegCerts stakeUnregCerts
    pure stakingList

stakeDelegCert ::
    C.ConwayEraOnwards era ->
    C.KeyHash 'C.StakePool C.StandardCrypto ->
    C.StakeCredential ->
    C.Certificate era
stakeDelegCert ceo stakePoolKeyHash stakeCred = do
    let stakePoolDelegatee = C.DelegStake $ C.conwayEraOnwardsConstraints ceo stakePoolKeyHash
        w1StakeDelgReqs = C.StakeDelegationRequirementsConwayOnwards ceo stakeCred stakePoolDelegatee
    C.makeStakeAddressDelegationCertificate w1StakeDelgReqs
