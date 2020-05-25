#!/bin/bash

if ! fc-list | grep -i sourcecodepro &>/dev/null; then
    ./install-source-code-pro-ubuntu.sh
fi
