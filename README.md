### Chang HardFork Testing

#### The testnet setup, nix configurations, and helper functions were imported from [Antaeus](https://github.com/IntersectMBO/antaeus/)

## Strategy
1. Integrate Plutus functions into basic validator scripts.
2. Lock some UTxOs within the script.
3. Generate a transaction to unlock the UTxOs, ensuring proper validation to test the functionality of the Plutus function.
4. Verify the final state to confirm that everything executed correctly.

## Current Tests

### Crypto 
- `verifyEcdsaSecp256k1Signature`
- `verifyEd25519Signature`
- `verifySchnorrSecp256k1Signature`
- `keccak_256`
- `blake2b_224`
- `bls12_381_g1`
  - `bls12_381_g1_compress`
  - `bls12_381_g1_uncompress`
  - `bls12_381_g1_scalarMul`
  - `bls12_381_g1_add`
  - `bls12_381_g1_neg`
  - `bls12_381_g1_equals`
- `bls12_381_g2`
  - `bls12_381_g2_compress`
  - `bls12_381_g2_uncompress`
  - `bls12_381_g2_scalarMul`
  - `bls12_381_g2_add`
  - `bls12_381_g2_neg`
  - `bls12_381_g2_equals`

### Reference Inputs
- Verify reference input address, value, and datum visibility using V3 script context

### Minting
- Minting an NFT with exactly max execution units

### Spending
- Locking and spending multiple UTxOs in/from the same script address in the same transaction
- Locking and spending multiple UTxOs in/from different script addresses in the same transaction
- Spending a locked UTxO from a script with multi-signature requirements

### Staking, Registration, Delegation, DeRegistration, and Retirement
- Registering multiple stake addresses in a single transaction
- Registering multiple stake pools in a single transaction
- Delegate to multiple stake pools in a single transaction
- DeRegistering multiple stake addresses in a single transaction
- Retiring multiple stake pools in a single transaction

### Efficiency Test
**PlutusV3 with plcVersion110** vs **PlutusV2 with plcVersion100**

*Efficiency was tested by comparing execution units*
| Test                                                   | Is efficient with PlutusV3 |
| ------------------------------------------------------ | -------------------------- |
| Minting NFT Script                                     | ✅                         |
| Spending locked UTxO with multi-signature requirements | ✅                         |

## Steps to run
1. `nix develop`
2. `cabal run changHardForkTesting`
