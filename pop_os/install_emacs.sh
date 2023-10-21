#!/bin/bash

# install emacs29
# https://arnesonium.com/2023/07/emacs-29-1-on-ubuntu-22-04-lts
sudo apt build-dep emacs
sudo apt install libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev \
    gnutls-bin libtree-sitter-dev gcc-10 imagemagick libmagick++-dev \
    libwebp-dev webp libxft-dev libxft2

export CC=/usr/bin/gcc-10
export CXX=/usr/bin/gcc-10

wget http://mirrors.tripadvisor.com/gnu/emacs/emacs-29.1.tar.gz

tar zxvf emacs-29.1.tar.gz

cd emacs-29.1
./autogen.sh
./configure --with-native-compilation=aot --with-imagemagick --with-json \
    --with-tree-sitter --with-xft
make -j$(nproc)

sudo make install

# Install emacs28
# sudo add-apt-repository ppa:kelleyk/emacs
# sudo apt update
# sudo apt install emacs28
#
