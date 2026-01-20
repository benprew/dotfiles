#!/bin/bash
set -e

HOSTNAME=$(hostname)

export PATH=$PATH:/usr/bin # restic lives in /usr/bin on fedora

export RESTIC_REPOSITORY="sftp:ben@nas.local:/mnt/nas/backup/$HOSTNAME"
export RESTIC_PASSWORD_FILE="$HOME/secrets/.restic_pass"

restic init --repo "$RESTIC_REPOSITORY" --verbose

# cleanup old snapshots
# restic forget --keep-weekly 10
# verify backup
# restic check --read-data
