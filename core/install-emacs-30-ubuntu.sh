#!/bin/bash

# see https://ubuntuhandbook.org/index.php/2023/08/gnu-emacs-29-1-ubuntu-ppa/
sudo add-apt-repository ppa:ubuntuhandbook1/emacs
sudo apt update
sudo apt remove --autoremove emacs emacs-common
sudo apt install emacs=1:30.1-0build1~ubuntu2204
