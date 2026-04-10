#!/bin/bash

mkdir -p ~/Mail/gmail
mkdir -p ~/org

# Create contacts file if it doesn't exist
if [ ! -f ~/org/contacts.org ]; then
    cp "$(dirname "$0")/contacts.org.example" ~/org/contacts.org
fi
