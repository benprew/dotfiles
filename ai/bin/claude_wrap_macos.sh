#!/bin/bash

# Check if we're in a git worktree and need to allow access to the main .git directory
EXTRA_ALLOW=""
if [ -f .git ]; then
  GITDIR=$(grep "gitdir:" .git | cut -d' ' -f2)
  if [ -n "$GITDIR" ]; then
    MAIN_GIT_DIR=$(dirname $(dirname "$GITDIR"))
    if [ -d "$MAIN_GIT_DIR" ]; then
      EXTRA_ALLOW="
(allow file-write*
  (subpath \"$MAIN_GIT_DIR\"))"
    fi
  fi
fi

PROFILE="(version 1)

;; Allow everything by default
(allow default)

;; Deny file writes globally, then allow specific paths
(deny file-write*
  (subpath \"$HOME\"))

;; Allow writes to specific paths
(allow file-write*
  (subpath \"$HOME/src/mage-go\")
  (subpath \"$HOME/.local/bin\")
  (subpath \"$HOME/.local/share/claude\")
  (literal \"$HOME/.claude.json\")
  (subpath \"$HOME/.claude\")
  (subpath \"$HOME/Library/Keychains\")
  (subpath \"$HOME/Library/Caches\")
  (subpath \"$(pwd -P)\")
  (subpath \"/private/tmp\")
  (subpath \"/tmp\"))

$EXTRA_ALLOW
"

unset ANTHROPIC_API_KEY
exec sandbox-exec -p "$PROFILE" \
  "$(which claude)" --permission-mode bypassPermissions
