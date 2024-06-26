{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Helpers.Committee where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C hiding (Voter)
import Cardano.Crypto.Hash.Class qualified
import Cardano.Ledger.Keys qualified as Keys
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust)

data Committee era = Committee
    { committeeColdSKey :: C.SigningKey C.CommitteeColdKey
    , commiteeColdVKey :: C.VerificationKey C.CommitteeHotKey
    , committeeColdKeyHash :: C.Hash C.CommitteeColdKey
    , committeeHotHash :: C.Hash C.CommitteeHotKey
    , committeeHotSKey :: C.SigningKey C.CommitteeHotKey
    , committeeHotKeyAuthCert :: C.Certificate era
    , committeeVoter :: C.Voter (C.EraCrypto (C.ShelleyLedgerEra era))
    }
    deriving (Show)

generateCommitteeKeysAndCertificate ::
    (MonadIO m) =>
    C.ConwayEraOnwards era ->
    m [Committee era]
generateCommitteeKeysAndCertificate ceo = do
    -- Generate multiple sets of committee cold keys and hot keys
    committeeColdSKeys <- mapM id $ take 4 $ repeat $ liftIO $ C.generateSigningKey C.AsCommitteeColdKey
    committeeHotSKeys <- mapM id $ take 4 $ repeat $ liftIO $ C.generateSigningKey C.AsCommitteeHotKey

    -- Helper function to generate a Committee from a pair of cold and hot keys
    let generateCommitteeHelper (coldSKey, hotSKey) = do
            let committeeColdVerificationKey@(C.CommitteeColdVerificationKey committeeColdVkey) =
                    C.getVerificationKey coldSKey
                committeeColdHash = C.verificationKeyHash committeeColdVerificationKey

                committeeHotVerificationKey@(C.CommitteeHotVerificationKey committeeHotVKey) =
                    C.getVerificationKey hotSKey
                _committeeHotHash = C.verificationKeyHash committeeHotVerificationKey
                -- Produce committee hot key authorization certificate
                ckh = C.conwayEraOnwardsConstraints ceo $ L.KeyHashObj $ Keys.hashKey committeeColdVkey
                hkh = C.conwayEraOnwardsConstraints ceo $ L.KeyHashObj $ Keys.hashKey committeeHotVKey
                committeeHotRequirements = C.CommitteeHotKeyAuthorizationRequirements ceo ckh hkh
                committeeHotKeyAuthCert = C.makeCommitteeHotKeyAuthorizationCertificate committeeHotRequirements

                -- Produce committee voter
                C.CommitteeHotKeyHash committeeHotHash = C.verificationKeyHash $ C.getVerificationKey hotSKey
                dRepVotingCredential = C.conwayEraOnwardsConstraints ceo $ C.KeyHashObj committeeHotHash
                committeeVoter = C.CommitteeVoter dRepVotingCredential

            return $
                Committee
                    coldSKey
                    committeeHotVerificationKey
                    committeeColdHash
                    _committeeHotHash
                    hotSKey
                    committeeHotKeyAuthCert
                    committeeVoter

    -- Generate the list of Committees
    committees <- mapM generateCommitteeHelper (zip committeeColdSKeys committeeHotSKeys)
    return committees

castCommittee :: C.SigningKey C.CommitteeHotKey -> C.SigningKey C.PaymentKey
castCommittee (C.CommitteeHotSigningKey committeeHotSK) = C.PaymentSigningKey committeeHotSK
