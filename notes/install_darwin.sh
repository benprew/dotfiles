#!/bin/bash

# Download hunspell dictionaries for US English
DICT_DIR="$(hunspell -D 2>&1 | grep -m1 '^\/')"
if [ -z "$DICT_DIR" ]; then
    DICT_DIR="/opt/homebrew/share/hunspell"
fi

mkdir -p "$DICT_DIR"

if [ ! -f "$DICT_DIR/en_US.dic" ]; then
    echo "Downloading hunspell en_US dictionaries..."
    curl -sL "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic" -o "$DICT_DIR/en_US.dic"
    curl -sL "https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff" -o "$DICT_DIR/en_US.aff"
    echo "Dictionaries installed to $DICT_DIR"
else
    echo "en_US dictionaries already installed"
fi
