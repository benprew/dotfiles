#!/bin/bash

if [[ ! -f ~/.emacs.d/.projectile ]]; then
    curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh
fi

# install fish
if ! grep -q fish /etc/shells; then
   echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
fi
if [[ $SHELL != "/usr/local/bin/fish" ]]; then
    chsh -s /usr/local/bin/fish
fi
