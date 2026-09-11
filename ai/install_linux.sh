#!/usr/bin/env bash

set -euo pipefail

if command -v dnf >/dev/null 2>&1; then
    if [[ $EUID -eq 0 ]]; then
        SUDO=()
    else
        SUDO=(sudo)
    fi

    "${SUDO[@]}" tee /etc/yum.repos.d/antigravity.repo >/dev/null <<'EOF'
[antigravity-rpm]
name=Antigravity RPM Repository
baseurl=https://us-central1-yum.pkg.dev/projects/antigravity-auto-updater-dev/antigravity-rpm
enabled=1
gpgcheck=0
EOF

    "${SUDO[@]}" dnf makecache
    "${SUDO[@]}" dnf install -y antigravity
fi

# install codex-cli agent
curl -fsSL https://chatgpt.com/codex/install.sh | sh
