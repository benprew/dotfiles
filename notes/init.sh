#!/bin/bash

if which brew 2>/dev/null; then
    brew services start syncthing
fi

if [[ ! -L ~/notes ]]; then
    ln -s ~/Sync/notes ~/notes
fi
