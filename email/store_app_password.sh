#!/usr/bin/env bash
set -euo pipefail

out="${MBSYNC_PASSWORD_FILE:-$HOME/.mbsync_password.gpg}"

if ! command -v gpg >/dev/null 2>&1; then
  echo "gpg is required but was not found in PATH" >&2
  exit 1
fi

recipient="${1:-}"
if [ -z "$recipient" ]; then
  default_recipient="$(
    gpg --list-secret-keys --with-colons 2>/dev/null |
      awk -F: '$1 == "uid" { print $10; exit }'
  )"

  if [ -n "$default_recipient" ]; then
    read -r -p "GPG recipient [$default_recipient]: " recipient
    recipient="${recipient:-$default_recipient}"
  else
    read -r -p "GPG recipient: " recipient
  fi
fi

if [ -z "$recipient" ]; then
  echo "No GPG recipient provided" >&2
  exit 1
fi

if [ -e "$out" ]; then
  read -r -p "$out already exists. Overwrite? [y/N] " overwrite
  case "$overwrite" in
    y|Y|yes|YES) ;;
    *) echo "Aborted"; exit 1 ;;
  esac
fi

read -r -s -p "Email app password: " password
printf '\n'
read -r -s -p "Confirm email app password: " confirm
printf '\n'

if [ "$password" != "$confirm" ]; then
  unset password confirm
  echo "Passwords did not match" >&2
  exit 1
fi

# Gmail displays app passwords in groups; those spaces are not part of the secret.
password="${password// /}"

tmp="$(mktemp "${TMPDIR:-/tmp}/mbsync-password.XXXXXX")"
trap 'rm -f "$tmp"' EXIT
chmod 600 "$tmp"
printf '%s' "$password" >"$tmp"
unset password confirm

gpg --yes --encrypt --recipient "$recipient" --output "$out" "$tmp"
chmod 600 "$out"

echo "Stored encrypted email app password at $out"
