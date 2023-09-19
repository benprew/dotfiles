#!/bin/bash

# for emacs org export on Ubuntu (uses /usr/bin/sensible-browser)
export BROWSER=firefox

if [[ ! -L ~/notes ]]; then
    ln -s ~/src/sync/notes ~/notes
fi
