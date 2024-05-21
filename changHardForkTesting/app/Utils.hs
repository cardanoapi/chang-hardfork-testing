{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
-- smart contract 1 liner
-- new functionality of plutus v3
{-# LANGUAGE NumericUnderscores #-}

module Utils where

import Cardano.Api
import Cardano.Api qualified as C
import Cardano.Api.Shelley hiding (Mainnet)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Short (ShortByteString)
import Data.Functor.Identity
import Data.IORef (IORef, readIORef)
import Data.Map qualified as Map
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog
import Hedgehog qualified as H
import Hedgehog.Gen hiding (map)
import Hedgehog.Internal.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Helpers.Common (toShelleyBasedEra)
import Helpers.Query qualified as Q
import Helpers.Test (assert, integrationRetryWorkspace, runTest)
import Helpers.TestData
import Helpers.TestResults (
    TestResult (..),
    TestSuiteResults (..),
    allFailureMessages,
    suiteFailureMessages,
    testSuitesToJUnit,
 )
import Helpers.Testnet qualified as TN
import Helpers.Tx qualified as Tx
import PlutusLedgerApi.V1.Bytes qualified as P
import PlutusTx.IsData.Class
import Test.Gen.Cardano.Api.Typed hiding (genStakeAddressReference, genStakeCredential)

data V3ScriptInfo = V3ScriptInfo
    { address :: Address ShelleyAddr
    , script :: Script PlutusScriptV3
    , hash :: ScriptHash
    , sbs :: ShortByteString
    }

data FunderInfo era = FunderInfo
    { input :: C.TxIn
    , collateral :: C.TxInsCollateral era
    , value :: C.Value
    , funderAddress :: Address ShelleyAddr
    , sKey :: SigningKey PaymentKey
    }

dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData = fromPlutusData $ toData sData

dataToHashableScriptData :: (ToData a1) => a1 -> HashableScriptData
dataToHashableScriptData sData = unsafeHashableScriptData (dataToScriptData sData)

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e) = error $ show e
    fromEither (Right b) = b
