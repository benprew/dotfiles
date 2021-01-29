#!/bin/bash

if [[ ! -e ~/bin/p4merge ]]; then
    wget http://filehost.perforce.com/perforce/r20.3/bin.linux26x86_64/p4v.tgz -O /tmp/p4v.tgz
    cd /tmp
    tar zxvf /tmp/p4v.tgz
    mv p4v-2020.3.2060285 ~/p4v
    ln -s ~/p4v/bin/p4merge ~/bin/p4merge
fi

git config --global merge.tool p4merge
git config --global mergetool.p4merge.path p4merge
