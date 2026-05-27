#!/bin/bash

set -euo pipefail

echo "Installing Claude Code (native)..."
curl -fsSL https://claude.ai/install.sh | bash

# installs to ~/.local/bin/claude, self-updating
