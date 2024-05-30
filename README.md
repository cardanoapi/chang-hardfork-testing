### Chang HardFork Testing

#### The testnet setup, nix configurations and Helper functions were imported from [Antaeus](https://github.com/IntersectMBO/antaeus/)

## Strategy
- Integrate Plutus functions into basic validator scripts.
- Lock some UTxO within the script.
- Generate a transaction to unlock the UTxO, ensuring proper validation to test the functionality of the Plutus function.
- Verify the final state to confirm that everything executed correctly.

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
- verify reference input address, value and datum visibility using V3 script context

### Minting
- minting a NFT with exactly max execution units

### Spending 
- Locking and spending multiple UTxOs in/from the same script address in the same transaction
- Locking and spending multiple UTxOs in/from different script address in the same transaction
- Spending a locked UTxO from a script fulfilling milti-signature requirements

### Staking, Registration, Delegattion and Reward Withdrawl 
- registering multiple stake address in a single transaction
- registering multiple stake pools in a single transaction
- delegate to multiple stake pools in a single transaction
- deRegistering multiple stake address in a single transaction

## Steps to run
1. `nix develop`
2. `cabal run changHardForkTesting\`