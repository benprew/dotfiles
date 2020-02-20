#!/bin/bash

if [[ ! -L ~/notes ]]; then
    ln -s ~/SparkleShare/github.com/sync/notes ~/notes
fi
