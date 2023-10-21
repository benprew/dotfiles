#!/bin/bash
set -e

HOSTNAME=$(hostname)

export PATH=$PATH:/usr/bin # restic lives in /usr/bin on fedora

export RESTIC_REPOSITORY="sftp:ben@nas.local:/mnt/nas/backup/$HOSTNAME"
export RESTIC_PASSWORD_FILE="$HOME/.restic_pass"

restic check --read-data
