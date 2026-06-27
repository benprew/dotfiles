#!/usr/bin/env bash
set -euo pipefail

maildir="${MU_MAILDIR:-$HOME/Mail/gmail}"
address="${MU_EMAIL_ADDRESS:-ben.prew@gmail.com}"

mkdir -p "$maildir"
mkdir -p "$HOME/org"

# Create contacts file if it doesn't exist
touch "$HOME/org/contacts.org"

if ! command -v mu >/dev/null 2>&1; then
  echo "mu is required but was not found in PATH" >&2
  exit 1
fi

if mu info >/dev/null 2>&1; then
  echo "Reinitializing mu database for $maildir"
  mu init --reinit
else
  echo "Initializing mu database for $maildir"
  mu init --maildir="$maildir" --my-address="$address"
fi
