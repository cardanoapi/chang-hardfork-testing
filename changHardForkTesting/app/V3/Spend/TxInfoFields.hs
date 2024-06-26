{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module V3.Spend.TxInfoFields where

import PlutusCore.Core (plcVersion110)
import PlutusLedgerApi.V1.Interval (after, before)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3
import PlutusTx (unsafeApplyCode)
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Lift (liftCode)
import PlutusTx.Prelude

data TxInfoFields = TxInfoFields
    { input :: TxOutRef
    , output :: Address
    , refInput :: TxOutRef
    , signers :: [PubKeyHash]
    , beforeRange :: POSIXTime
    , afterRange :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''TxInfoFields
PlutusTx.makeLift ''TxInfoFields

-- TODO: Extra Filds Not Included
-- txInfoVotes
-- txInfoProposalProcedures
-- txInfoCurrentTreasuryAmount
-- txInfoTreasuryDonation
-- txInfoId
-- txInfoData
-- txInfoRedeemers
-- txInfoWdrl
-- txInfoTxCerts

{-# INLINEABLE mkValidator #-}
mkValidator :: TxInfoFields -> () -> () -> [TxInInfo] -> [TxOut] -> [TxInInfo] -> Value -> POSIXTimeRange -> [PubKeyHash] -> Lovelace -> Bool
mkValidator fields dat red inputs outputs refInputs mint range signatories fee =
    hasInput
        && hasOutput
        && hasMint
        && hasRefInput
        -- && isBefore
        -- && isAfter
        && hasSignatures
        && hasFee
  where
    hasInput = input fields `elem` (map txInInfoOutRef $ inputs)
    hasOutput = output fields `elem` (map txOutAddress $ outputs)
    hasRefInput = refInput fields `elem` (map txInInfoOutRef $ refInputs)
    hasFee = fee < 1_000_000
    hasMint = not $ null $ flattenValue $ mint
    isBefore = beforeRange fields `before` range
    isAfter = afterRange fields `after` range
    hasSignatures = all (\x -> x `elem` signatories) (nub $ signers fields)

mkWrappedValidator :: TxInfoFields -> BuiltinData -> BuiltinUnit
mkWrappedValidator fields ctx_ =
    check
        $ mkValidator
            fields
            (unsafeFromBuiltinData dat_)
            (unsafeFromBuiltinData red_)
            (unsafeFromBuiltinData inputs)
            (unsafeFromBuiltinData outputs)
            (unsafeFromBuiltinData refInputs)
            (unsafeFromBuiltinData mint)
            (unsafeFromBuiltinData range)
            (unsafeFromBuiltinData signatories)
            (unsafeFromBuiltinData fee)
  where
    ds :: BuiltinData -> BI.BuiltinList BuiltinData
    ds bd = BI.snd (BI.unsafeDataAsConstr bd)

    context = ds ctx_
    red_ = BI.head $ BI.tail context

    dat_ = red_ -- TODO : Filter out datum from context
    txInfo = BI.head context
    inputs = BI.head $ ds txInfo
    refInputs = BI.head $ BI.tail $ ds txInfo
    outputs = BI.head $ BI.tail $ BI.tail $ ds txInfo
    mint = BI.head $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ ds txInfo
    range = BI.head $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ BI.tail $ ds txInfo
    signatories =
        BI.head
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ BI.tail
            $ ds txInfo
    fee = BI.head $ BI.tail $ BI.tail $ BI.tail $ ds txInfo

validator :: TxInfoFields -> PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
validator fields = $$(PlutusTx.compile [||mkWrappedValidator||]) `unsafeApplyCode` liftCode plcVersion110 fields
