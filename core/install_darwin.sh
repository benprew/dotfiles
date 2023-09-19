#!/bin/bash

FISH=/usr/local/bin/fish

# install fish
if ! grep -q $FISH /etc/shells; then
   echo "$FISH" | sudo tee -a /etc/shells
fi
if [[ $SHELL != "$FISH" ]]; then
    chsh -s "$FISH"
fi

# change location for screenshot downloads
defaults write com.apple.screencapture location ~/Downloads/
