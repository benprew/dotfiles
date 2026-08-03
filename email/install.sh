#!/usr/bin/env bash
set -euo pipefail

maildir="${MU_MAILDIR:-$HOME/Mail/gmail}"
addresses="${MU_EMAIL_ADDRESSES:-ben@throwingbones.com ben.prew@gmail.com}"

mkdir -p "$maildir"
mkdir -p "$HOME/org"

# Create contacts file if it doesn't exist
touch "$HOME/org/contacts.org"

if ! command -v mu >/dev/null 2>&1; then
  echo "mu is required but was not found in PATH" >&2
  exit 1
fi

echo "Initializing mu database for $maildir"
read -r -a address_list <<< "$addresses"
address_args=()
for address in "${address_list[@]}"; do
  address_args+=(--personal-address="$address")
done
mu init --maildir="$maildir" "${address_args[@]}"
