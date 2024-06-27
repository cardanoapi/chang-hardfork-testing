
# Check if .cluster folder exists and remove it if it does
if [ -d ".cluster" ]; then
    echo "Removing existing .cluster folder"
    rm -rf .cluster
fi

if [ -d ".shelleyWallets" ]; then
    echo "Removing existing .shelleyWallets folder"
    rm -rf .shelleyWallets
fi

if [ -d ".stakePools" ]; then
    echo "Removing existing .stakePools folder"
    rm -rf .stakePools
fi

if [ -d ".govStateTracking" ]; then
    echo "Removing existing .govStateTracking folder"
    rm -rf .govStateTracking
fi

# Create .cluster folder
echo "Creating .cluster folder"
mkdir .cluster

# Create .shelleyWallets folder
echo "Creating .shelleyWallets folder"
mkdir .shelleyWallets

echo "Creating .stakePools folder"
mkdir .stakePools

echo "Creating .govStateTracking folder"
mkdir .govStateTracking
mkdir .govStateTracking/committee
mkdir .govStateTracking/committeeNoConfidence
mkdir .govStateTracking/constitution
mkdir .govStateTracking/info
mkdir .govStateTracking/motionNoConfidence
mkdir .govStateTracking/pParamUpdate

export START_TESTNET=1

# Run cabal command
cabal run changHardForkTesting/