#!/bin/bash

if grep -q -i "ubuntu" /etc/os-release; then
  OS="Ubuntu"
  VERSION_ID=$(grep -oP '(?<=VERSION_ID=")\d+(\.\d+)?(?=")' /etc/os-release)
else
  OS=""
fi

# Determine if the --break-system-packages flag should be used
BREAK_SYSTEM_PACKAGES_FLAG=""
if [[ "$OS" == "Ubuntu" ]] && [ "$(echo "$VERSION_ID >= 24.04" | bc -l)" -eq 1 ]; then
  BREAK_SYSTEM_PACKAGES_FLAG="--break-system-packages"
fi

# Install aider-install with the determined flag
python3 -m pip install aider-install $BREAK_SYSTEM_PACKAGES_FLAG

aider-install

ai/install_gemini.sh
