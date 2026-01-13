#!/bin/bash

# Use JetBrainsMono font
if ! fc-list | grep -i jetbrainsmono &>/dev/null; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"
fi

./core/install-source-code-pro-ubuntu.sh

YT_DLP=~/bin/yt-dlp
if [[ ! -e "$YT_DLP" ]]; then
    mkdir -p ~/bin
    curl -L https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp -o "$YT_DLP"
    chmod +x "$YT_DLP"
fi

./core/install-powertop.sh
