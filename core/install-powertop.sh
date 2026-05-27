#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ -e /etc/systemd/system/powertop.service ]; then
   echo "Powertop already installed"
   exit 0
fi

SYSD_DIR=/etc/systemd/system/
mkdir -p "$SYSD_DIR"
sudo cp "$SCRIPT_DIR/powertop.service" "$SYSD_DIR"
sudo systemctl enable powertop.service
