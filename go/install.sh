#!/bin/bash

set -e

GOVERSION="1.25.1"

# Detect architecture
ARCH=$(uname -m)
case $ARCH in
    x86_64)
        GOARCH="amd64"
        ;;
    aarch64|arm64)
        GOARCH="arm64"
        ;;
    armv7l|armv6l)
        GOARCH="armv6l"
        ;;
    *)
        echo "Unsupported architecture: $ARCH"
        exit 1
        ;;
esac

if ! (command -v go &>/dev/null && go version |grep $GOVERSION); then
    echo "Installing go$GOVERSION for linux-$GOARCH"
    cd "$(mktemp -d)" || exit 1
    wget "https://go.dev/dl/go$GOVERSION.linux-$GOARCH.tar.gz"
    sudo rm -rf /usr/local/go && sudo tar -C /usr/local -xzf go$GOVERSION.linux-$GOARCH.tar.gz
fi

export PATH=$PATH:/usr/local/go/bin

echo "Installing gopls"
go install golang.org/x/tools/gopls@latest
