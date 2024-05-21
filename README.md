### Chang HardFork Testing

#### The testnet cluster setup, nix configurations and Helper functions were imported from [Antaeus](https://github.com/IntersectMBO/antaeus/)

## Strategy
- Integrate Plutus functions into basic validator scripts.
- Lock some UTxO within the script.
- Generate a transaction to unlock the UTxO, ensuring proper validation to test the functionality of the Plutus function.
- Verify the final state to confirm that everything executed correctly.

## Current Tests
- Test the `verifySchnorrSecp256k1Signature` function.
- Test the `keccak_256` function.

## Steps to run
1. `nix develop`
2. `cabal run changHardForkTesting\`