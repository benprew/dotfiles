#!/bin/bash

if [ -e /etc/systemd/system/powertop.service ]; then
   echo "Powertop already installed"
   exit 0
fi

SYSD_DIR=/etc/systemd/system/
mkdir -p "$SYSD_DIR"
sudo cp powertop.service "$SYSD_DIR"
sudo systemctl enable powertop.service
