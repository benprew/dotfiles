#!/bin/bash

set -e

GOVERSION="1.21.1"

if ! command -v go &>/dev/null; then
    echo "Installing go$GOVERSION"
    cd $(mktemp -d) || exit 1
    wget "https://go.dev/dl/go$GOVERSION.linux-amd64.tar.gz"
    sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go$GOVERSION.linux-amd64.tar.gz
fi

export PATH=$PATH:/usr/local/go/bin

echo "Installing gopls"
go install golang.org/x/tools/gopls@latest
