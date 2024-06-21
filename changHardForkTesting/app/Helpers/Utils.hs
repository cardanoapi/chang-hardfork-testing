{-# LANGUAGE DataKinds #-}

module Helpers.Utils where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Conway.Governance qualified as CG
import Control.Lens.Getter ((^.))
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import Data.List (intersperse)
import Data.Time.Clock.POSIX qualified as Time
import Debug.Trace qualified as Debug
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Hedgehog.Extras qualified as HE
import Hedgehog.Extras.Stock.CallStack qualified as H
import Helpers.Common (makeAddress)
import Helpers.PlutusScripts (mintScriptWitness', plutusL3, toScriptData)
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.IO qualified as IO
import System.IO.Temp qualified as IO
import System.Info qualified as IO
import System.Random (randomRIO)

-- | Right from Either or throw Left error
unsafeFromRight :: (Show l) => Either l r -> r
unsafeFromRight (Left err) = error (show err)
unsafeFromRight (Right value) = value

{- | This is a copy of the workspace from
hedgehog-extras:Hedgehog.Extras.Test.Base, which for darwin sets
the systemTemp folder to /tmp.

It creates a temporary folder with @prefixPath@, which is removed
after the supplied function @f@ returns.
-}
workspace :: (MonadTest m, MonadIO m, GHC.HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace prefixPath f = GHC.withFrozenCallStack $ do
    systemTemp <- case IO.os of
        "darwin" -> pure "/tmp"
        _ -> H.evalIO IO.getCanonicalTemporaryDirectory
    maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
    let systemPrefixPath = systemTemp <> "/" <> prefixPath
    H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
    ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
    H.annotate $ "Workspace: " <> ws
    liftIO $ IO.writeFile (ws <> "/module") H.callerModuleName
    f ws
    when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
        H.evalIO $ IO.removeDirectoryRecursive ws

{- | Read file text envelope as a specific type (e.g. C.VerificationKey C.GenesisUTxOKey)
  and throw error on failure
-}
readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> C.File content 'C.In -> m a
readAs as path = do
    H.annotate $ C.unFile path
    H.leftFailM . liftIO $ C.readFileTextEnvelope as path

-- | Same as readAs but return Nothing on error
maybeReadAs ::
    (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> C.File content 'C.In -> m (Maybe a)
maybeReadAs as file@(C.File fp) = do
    H.annotate fp
    maybeEither . liftIO $ C.readFileTextEnvelope as file
  where
    maybeEither m = m >>= return . either (const Nothing) Just

-- | Concatenate Just Strings. Useful for aggregating test failures.
concatMaybes :: (MonadTest m) => [Maybe String] -> m (Maybe String)
concatMaybes mList =
    let justStrings = map (maybe "" id) mList
        withLineBreaks = mconcat . intersperse "\n\n" $ justStrings
     in if all null justStrings then pure Nothing else pure $ Just withLineBreaks

-- | Convert a 'POSIXTime' to the number of milliseconds since the Unix epoch.
posixToMilliseconds :: Time.POSIXTime -> Integer
posixToMilliseconds posixTime = round $ 1000 * (realToFrac posixTime :: Double)

-- | Fails any Left returning Rights.
anyLeftFail :: (MonadTest m, Show e) => m [Either e a] -> m [a]
anyLeftFail es = mapM (HE.leftFailM . return) =<< es

-- | Fails any Left returning unit.
anyLeftFail_ :: (MonadTest m, Show e) => m [Either e a] -> m ()
anyLeftFail_ = void . anyLeftFail

pickRandomElements :: (MonadTest m, MonadIO m) => Int -> [a] -> m [a]
pickRandomElements n xs = do
    indices <- replicateM n $ randomRIO (0, length xs - 1)
    return [xs !! i | i <- indices]

getChunk :: Int -> Int -> [a] -> [a]
getChunk chunkSize chunkIndex = take chunkSize . drop (chunkSize * chunkIndex)

getVkey :: C.SigningKey C.PaymentKey -> C.VerificationKey C.PaymentKey
getVkey skey = C.getVerificationKey skey

paymentKeyToAddress :: C.SigningKey C.PaymentKey -> C.NetworkId -> C.Address C.ShelleyAddr
paymentKeyToAddress skey netId = makeAddress (Left $ getVkey skey) netId

data GovPurpose era = GovPurpose
    { udpateCommittee :: C.StrictMaybe (L.GovPurposeId 'L.CommitteePurpose era)
    , pParamUpdate :: C.StrictMaybe (L.GovPurposeId 'L.PParamUpdatePurpose era)
    , hardFork :: C.StrictMaybe (L.GovPurposeId 'L.HardForkPurpose era)
    , constitution :: C.StrictMaybe (L.GovPurposeId 'L.ConstitutionPurpose era)
    }

getPrevGovAction oldGovState = do
    let prevEnactedGovActions = oldGovState ^. CG.proposalsGovStateL ^. CG.pRootsL
        prevUpdateCommittee = CG.prRoot $ CG.grCommittee prevEnactedGovActions
        prevPParamUpdate = CG.prRoot $ CG.grPParamUpdate prevEnactedGovActions
        prevHardFork = CG.prRoot $ CG.grHardFork prevEnactedGovActions
        prevConstitution = CG.prRoot $ CG.grConstitution prevEnactedGovActions
    GovPurpose prevUpdateCommittee prevPParamUpdate prevHardFork prevConstitution

addEpoch e i = L.EpochNo $ L.unEpochNo e + i
