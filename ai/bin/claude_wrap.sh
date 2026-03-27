#!/bin/bash

# export NODE_PATH=$HOME/node_modules

# Check if we're in a git worktree and need to bind the main .git directory
EXTRA_BINDS=""
if [ -f .git ]; then
  # This is a worktree - .git is a file pointing to the main repo
  # Extract the gitdir path from the .git file
  GITDIR=$(grep "gitdir:" .git | cut -d' ' -f2)
  if [ -n "$GITDIR" ]; then
    # Get the main repository's .git directory (remove /worktrees/... suffix)
    MAIN_GIT_DIR=$(dirname $(dirname "$GITDIR"))
    if [ -d "$MAIN_GIT_DIR" ]; then
      EXTRA_BINDS="--bind $MAIN_GIT_DIR $MAIN_GIT_DIR"
    fi
  fi
fi

  # --ro-bind "$NODE_PATH" "$NODE_PATH" \
  # --setenv NODE_PATH "$NODE_PATH" \
exec bwrap \
  --ro-bind /usr /usr \
  --ro-bind /lib /lib \
  --ro-bind /etc /etc \
  --ro-bind /run /run \
  --proc /proc \
  --dev /dev \
  --symlink usr/lib64 /lib64 \
  --tmpfs /tmp \
  --unshare-all \
  --share-net \
  --die-with-parent \
  --new-session \
  --bind "$HOME/.local/bin" "$HOME/.local/bin" \
  --bind "$HOME/.local/share/claude" "$HOME/.local/share/claude" \
  --bind "$HOME/.claude.json" "$HOME/.claude.json" \
  --bind "$HOME/.claude" "$HOME/.claude" \
  --bind "$HOME/src/mage-go" "$HOME/src/mage-go" \
  --bind "$(pwd)" "$(pwd)" \
  $EXTRA_BINDS \
  --chdir "$(pwd)" \
  --unsetenv ANTHROPIC_API_KEY \
  "$HOME/.local/bin/claude" --permission-mode bypassPermissions
