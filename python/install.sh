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

pip3 install --user black flake8 python-language-server $BREAK_SYSTEM_PACKAGES_FLAG
