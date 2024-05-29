{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.StakePool where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Shelley qualified as C hiding (Voter)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Ratio ((%))

data StakePool era = StakePool
    { sPSKey :: C.SigningKey C.StakePoolKey
    , sPVrfKey :: C.SigningKey C.VrfKey
    , sPRewardKey :: C.SigningKey C.StakeKey
    , sPPoolId :: C.PoolId
    , sPStakeKeyHash :: C.Hash C.StakeKey
    , sPLedgerKeyHash :: C.KeyHash 'C.StakePool C.StandardCrypto
    , sPStakeCred :: C.StakeCredential
    , sPRegCert :: C.Certificate era
    , sPVoter :: C.Voter (C.EraCrypto (C.ShelleyLedgerEra era))
    }
    deriving (Show)

generateStakePoolKeyCredentialsAndCertificate ::
    (MonadIO m) =>
    C.ConwayEraOnwards era ->
    C.NetworkId ->
    m [(StakePool era)]
generateStakePoolKeyCredentialsAndCertificate ceo networkId = do
    stakePoolSkey1 <- liftIO $ C.generateSigningKey C.AsStakePoolKey
    stakePoolVrfKey1 <- liftIO $ C.generateSigningKey C.AsVrfKey
    stakePoolRewardKey1 <- liftIO $ C.generateSigningKey C.AsStakeKey
    stakePoolSkey2 <- liftIO $ C.generateSigningKey C.AsStakePoolKey
    stakePoolVrfKey2 <- liftIO $ C.generateSigningKey C.AsVrfKey
    stakePoolRewardKey2 <- liftIO $ C.generateSigningKey C.AsStakeKey
    stakePoolSkey3 <- liftIO $ C.generateSigningKey C.AsStakePoolKey
    stakePoolVrfKey3 <- liftIO $ C.generateSigningKey C.AsVrfKey
    stakePoolRewardKey3 <- liftIO $ C.generateSigningKey C.AsStakeKey
    let
        makeStakePool stakePoolSkey stakePoolVrfKey stakePoolRewardKey =
            let
                stakePoolOwnerVKey = C.getVerificationKey stakePoolRewardKey
                stakePoolOwnerVKeyHash = C.verificationKeyHash stakePoolOwnerVKey
                stakePoolKeyHash@(C.StakePoolKeyHash ledgerStakePoolKeyHash) =
                    C.verificationKeyHash $ C.getVerificationKey stakePoolSkey
                stakePoolVrfKeyHash@(C.VrfKeyHash _vrfKeyHash) = C.verificationKeyHash $ C.getVerificationKey stakePoolVrfKey
                stakePoolStakeCred = C.StakeCredentialByKey (C.verificationKeyHash stakePoolOwnerVKey)
                rewardAccountAddr = C.makeStakeAddress networkId stakePoolStakeCred
                stakePoolParams =
                    C.StakePoolParameters
                        { C.stakePoolId = stakePoolKeyHash
                        , C.stakePoolVRF = stakePoolVrfKeyHash
                        , C.stakePoolCost = C.Lovelace 0
                        , C.stakePoolMargin = 0 % 1
                        , C.stakePoolRewardAccount = rewardAccountAddr
                        , C.stakePoolPledge = 0
                        , C.stakePoolOwners = [stakePoolOwnerVKeyHash]
                        , C.stakePoolRelays = [C.StakePoolRelayIp Nothing Nothing Nothing]
                        , C.stakePoolMetadata = Nothing
                        }
                ledgerStakePoolParams = C.conwayEraOnwardsConstraints ceo $ C.toShelleyPoolParams stakePoolParams
                stakePoolRegReq = C.StakePoolRegistrationRequirementsConwayOnwards ceo ledgerStakePoolParams
                stakePoolRegistrationCert = C.makeStakePoolRegistrationCertificate stakePoolRegReq
                stakePoolVoter = C.StakePoolVoter $ C.conwayEraOnwardsConstraints ceo ledgerStakePoolKeyHash
             in
                StakePool
                    stakePoolSkey
                    stakePoolVrfKey
                    stakePoolRewardKey
                    stakePoolKeyHash
                    stakePoolOwnerVKeyHash
                    ledgerStakePoolKeyHash
                    stakePoolStakeCred
                    stakePoolRegistrationCert
                    stakePoolVoter
        stakePool1 = makeStakePool stakePoolSkey1 stakePoolVrfKey1 stakePoolRewardKey1
        stakePool2 = makeStakePool stakePoolSkey2 stakePoolVrfKey2 stakePoolRewardKey2
        stakePool3 = makeStakePool stakePoolSkey3 stakePoolVrfKey3 stakePoolRewardKey3
    return $ [stakePool1, stakePool2, stakePool3]

makeStakePoolRetireCertification ::
    C.ConwayEraOnwards era -> StakePool era -> C.EpochNo -> C.Certificate era
makeStakePoolRetireCertification ceo stakePool epochNo = do
    let
        stakePoolRRetireReqs = C.StakePoolRetirementRequirementsConwayOnwards ceo (sPPoolId stakePool) epochNo
    C.makeStakePoolRetirementCertificate stakePoolRRetireReqs
