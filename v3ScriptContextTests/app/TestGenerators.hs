-- smart contract 1 liner 
-- new functionality of plutus v3 
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
module TestGenerators where
import Test.Gen.Cardano.Api.Typed hiding (genStakeAddressReference, genStakeCredential)
import Cardano.Api
import Cardano.Api.Shelley hiding (Mainnet)
import Hedgehog
import Data.Functor.Identity
import Hedgehog.Gen hiding (map)
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Gen as Gen
import qualified Data.Map as Map
import Helpers.Testnet qualified as TN
import Data.IORef (IORef, readIORef)
import Helpers.TestResults (
  TestResult (..),
  TestSuiteResults (..),
  allFailureMessages,
  suiteFailureMessages,
  testSuitesToJUnit,
 )
import Helpers.Test (integrationRetryWorkspace, runTest, assert)
import Data.Time.Clock.POSIX qualified as Time
import Hedgehog qualified as H
import Helpers.TestData 
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Helpers.Tx as Tx
import Helpers.Common (toShelleyBasedEra)
import qualified Cardano.Api as C
import qualified Helpers.Query as Q
import PlutusTx.IsData.Class
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString as BS
import qualified PlutusLedgerApi.V1.Bytes as P

data V3ScriptInfo = V3ScriptInfo {
    address :: Address ShelleyAddr,
    script:: Script PlutusScriptV3,
    hash :: ScriptHash,
    sbs :: ShortByteString
}

data FunderInfo era = FunderInfo {
  input :: C.TxIn,
  collateral :: C.TxInsCollateral era,
  value:: C.Value,
  funderAddress :: Address ShelleyAddr,
  sKey :: SigningKey PaymentKey
}

dataToScriptData :: (ToData a1) => a1 -> ScriptData
dataToScriptData sData = fromPlutusData $ toData sData

dataToHashableScriptData :: (ToData a1) => a1  -> HashableScriptData
dataToHashableScriptData sData = unsafeHashableScriptData (dataToScriptData sData)

bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e) = error $ show e
    fromEither (Right b) = b