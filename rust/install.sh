#!/bin/bash

set -euo pipefail

# Fedora's toolchain and editor tools come from fedora-packages.txt.
# The repository installer installs that manifest before running this script.
if [[ -f /etc/fedora-release ]]; then
    exit 0
fi

# OS prerequisites are installed by the module package manifests first.
if [[ "$(uname -s)" == Darwin ]]; then
    if ! xcrun --find cc >/dev/null 2>&1; then
        echo "Install Apple's command-line developer tools with xcode-select --install, then rerun this installer." >&2
        exit 1
    fi
fi

export PATH="${CARGO_HOME:-$HOME/.cargo}/bin:$PATH"
if ! command -v rustup >/dev/null 2>&1; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs |
        sh -s -- -y --profile default
fi

# Preserve an existing default toolchain, independently of directory overrides.
if default_toolchain=$(rustup default); then
    default_toolchain=${default_toolchain%% *}
else
    rustup default stable
    default_toolchain=stable
fi
rustup component add --toolchain "$default_toolchain" rust-analyzer rust-src rustfmt clippy
