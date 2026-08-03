#!/bin/bash

# 1. Define installation paths
INSTALL_DIR="$HOME/.codex-cli-app"
BIN_DIR="$HOME/.local/bin"

echo "------------------------------------------------"
echo "Installing Official OpenAI Codex CLI..."
echo "------------------------------------------------"

# 2. Check for Node.js
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is not installed. Please install Node.js 22+ first."
    exit 1
fi

NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 22 ]; then
    echo "Error: Codex requires Node.js 22 or later. Current version: $(node -v)"
    exit 1
fi

# 3. Create directories
mkdir -p "$INSTALL_DIR"
mkdir -p "$BIN_DIR"

# 4. Initialize with a VALID name
cd "$INSTALL_DIR" || exit
echo '{"name": "codex-local-install", "version": "1.0.0"}' > package.json

# 5. Install the CLI
echo "Downloading @openai/codex from NPM..."
npm install @openai/codex

# 6. Create the symlink
# Note: npm installs the binary into node_modules/.bin/
ln -sf "$INSTALL_DIR/node_modules/.bin/codex" "$BIN_DIR/codex"

npm install @agentclientprotocol/codex-acp
ln -sf "$INSTALL_DIR/node_modules/.bin/codex-acp" "$BIN_DIR/codex-acp"

echo "------------------------------------------------"
echo "Installation Successful!"
echo "------------------------------------------------"
