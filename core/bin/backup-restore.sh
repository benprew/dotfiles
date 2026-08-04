#!/bin/bash
set -e

HOSTNAME=$1

export RESTIC_REPOSITORY="sftp:ben@nas:/mnt/nas/backup/$HOSTNAME"
export RESTIC_PASSWORD_FILE="$HOME/secrets/.restic_pass"

restic restore latest --verbose --target .
