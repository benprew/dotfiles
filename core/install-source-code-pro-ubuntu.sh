#!/bin/bash
set  -euo pipefail

FONT_HOME=~/.local/share/fonts

if fc-list | grep -i sourcecodepro &>/dev/null; then
   echo "font Sourcecode Pro already installed."
   exit 0
fi

mkdir -p /tmp/adodefont
cd /tmp/adodefont
wget -q --show-progress -O source-code-pro.zip https://github.com/adobe-fonts/source-code-pro/archive/2.030R-ro/1.050R-it.zip
unzip -q source-code-pro.zip -d source-code-pro
mkdir -p $FONT_HOME
cp -v source-code-pro/*/OTF/*.otf $FONT_HOME
fc-cache -f
rm -rf source-code-pro{,.zip}
