#!/bin/bash

# 1. Define installation paths
INSTALL_DIR="$HOME/.gemini-cli-app"
BIN_DIR="$HOME/.local/bin"

echo "------------------------------------------------"
echo "Installing Official Google Gemini CLI..."
echo "------------------------------------------------"

# 2. Check for Node.js
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is not installed. Please install Node.js 20+ first."
    exit 1
fi

# 3. Create directories
mkdir -p "$INSTALL_DIR"
mkdir -p "$BIN_DIR"

# 4. Initialize with a VALID name (no leading dots)
cd "$INSTALL_DIR" || exit
echo '{"name": "gemini-local-install", "version": "1.0.0"}' > package.json

# 5. Install the CLI
echo "Downloading @google/gemini-cli from NPM..."
npm install @google/gemini-cli

# 6. Create the symlink
# Note: npm installs the binary into node_modules/.bin/
ln -sf "$INSTALL_DIR/node_modules/.bin/gemini" "$BIN_DIR/gemini"

# 7. Check if ~/.local/bin is in the PATH
if [[ ":$PATH:" != *":$BIN_DIR:"* ]]; then
    echo ""
    echo "Adding $BIN_DIR to your .bashrc..."
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    echo "Please run 'source ~/.bashrc' after this script finishes."
fi

echo "------------------------------------------------"
echo "Installation Successful!"
echo "------------------------------------------------"
