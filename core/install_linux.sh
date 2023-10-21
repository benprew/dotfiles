#!/bin/bash

# Use JetBrainsMono instead
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"
# if ! fc-list | grep -i sourcecodepro &>/dev/null; then
#     ./install-source-code-pro-ubuntu.sh
# fi

# fedora >= 26 includes "Night Light"
# if ! pgrep redshift-gtk; then
#     echo "starting redshift night-light, it should show up in the taskbar"
#     redshift-gtk &
# fi


YT_DLP=~/bin/yt-dlp
if [[ ! -e "$YT_DLP" ]]; then
    curl -L https://github.com/yt-dlp/yt-dlp/releases/latest/download/yt-dlp -o "$YT_DLP"
    chmod +x "$YT_DLP"
fi
