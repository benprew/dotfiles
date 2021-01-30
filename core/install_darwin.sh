#!/bin/bash

# install fish
if ! grep -q fish /etc/shells; then
   echo "/usr/local/bin/fish" | sudo tee -a /etc/shells
fi
if [[ $SHELL != "/usr/local/bin/fish" ]]; then
    chsh -s /usr/local/bin/fish
fi

# change location for screenshot downloads
defaults write com.apple.screencapture location ~/Downloads/
