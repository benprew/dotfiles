#!/bin/bash

if ! fc-list | grep -i sourcecodepro &>/dev/null; then
    ./install-source-code-pro-ubuntu.sh
fi

echo "starting redshift night-light in background, it should show up in your taskbar"
redshift-gtk &
