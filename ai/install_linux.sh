#!/bin/bash

if grep -q -i "ubuntu" /etc/os-release; then
  OS="Ubuntu"
else
  OS=""
fi

NODE_VERSION=$( (nodejs --version 2>/dev/null || node --version 2>/dev/null) | grep -oP '\d+' | head -n 1)
# install nodejs 20 (needed for gemini)
if [[ "$OS" == "Ubuntu" ]] && [ "$(echo "$NODE_VERSION >= 20" | bc -l)" -eq 0 ]; then
    curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
    sudo apt install nodejs -y
fi

ai/install_gemini.sh
ai/install_codex.sh
ai/install_claude.sh
