#!/bin/bash

sudo apt remove --autoremove emacs emacs-common
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs28
