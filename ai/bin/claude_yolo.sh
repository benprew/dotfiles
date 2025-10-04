#!/bin/bash

export NODE_PATH=$HOME/node_modules
exec bwrap \
  --ro-bind /usr /usr \
  --ro-bind /etc /etc \
  --ro-bind /run /run \
  --ro-bind "$NODE_PATH" "$NODE_PATH" \
  --proc /proc \
  --dev /dev \
  --symlink usr/lib64 /lib64 \
  --tmpfs /tmp \
  --unshare-all \
  --share-net \
  --die-with-parent \
  --new-session \
  --bind "$HOME/bin" "$HOME/bin" \
  --bind "$HOME/.claude.json" "$HOME/.claude.json" \
  --bind "$HOME/.claude" "$HOME/.claude" \
  --bind "$(pwd)" "$(pwd)" \
  --chdir "$(pwd)" \
  --setenv NODE_PATH "$NODE_PATH" \
  --unsetenv ANTHROPIC_API_KEY \
  "$HOME/bin/claude" --print --permission-mode bypassPermissions "$@"
