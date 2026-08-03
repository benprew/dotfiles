#!/bin/bash

# Install uv package manager
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install Python development tools managed by uv
"$HOME/.local/bin/uv" tool install black
"$HOME/.local/bin/uv" tool install flake8
"$HOME/.local/bin/uv" tool install python-language-server
"$HOME/.local/bin/uv" tool install ty
"$HOME/.local/bin/uv" tool install ruff
