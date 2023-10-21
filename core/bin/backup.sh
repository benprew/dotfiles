#!/bin/bash
set -e

HOSTNAME=$(hostname)

export PATH=$PATH:/usr/bin # restic lives in /usr/bin on fedora

export RESTIC_REPOSITORY="sftp:ben@nas.local:/mnt/nas/backup/$HOSTNAME"
export RESTIC_PASSWORD_FILE="$HOME/.restic_pass"

restic backup "$HOME" --verbose \
       -e "$HOME/.wine" \
       -e "$HOME/restore" \
       -e "$HOME/.local" \
       -e "$HOME/.cache" \
       -e "$HOME/Games" \
       -e "$HOME/.var" \
       -e "$HOME/.cargo" \
       -e "$HOME/.mozilla" \
       -e "$HOME/.log" \
       -e "$HOME/.npm"
# cleanup old snapshots
# restic forget --keep-weekly 10
# verify backup
# restic check --read-data
