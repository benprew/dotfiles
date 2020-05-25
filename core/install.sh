#!/bin/bash

if [[ ! -f ~/.emacs.d/prelude-cheatsheet.pdf ]]; then
    # install prelude
    curl -L https://git.io/epre | sh
fi
