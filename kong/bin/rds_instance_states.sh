#!/bin/bash

cd ~/src/pints/master || exit 1

STORY_IDS=$(aws rds describe-db-instances --query DBInstances[].DBInstanceIdentifier | egrep -po '[0-9]{8,}')
for s in $STORY_IDS; do
  STATE=$(tracker story "$s" | jq -r '.current_state')
  if [[ $STATE = 'accepted' ]]; then
    BRANCH=$(git ls-remote --heads 2>&1 |grep "$s")
    echo "$STATE $s $BRANCH";
  else
    echo "$STATE $s"
  fi
done
