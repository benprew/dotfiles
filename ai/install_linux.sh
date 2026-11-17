#!/usr/bin/env bash

set -euo pipefail

# install antigravity-cli agent
curl -fsSL https://antigravity.google/cli/install.sh | bash

# install codex-cli agent
curl -fsSL https://chatgpt.com/codex/install.sh | sh
