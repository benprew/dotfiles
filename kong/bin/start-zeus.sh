#!/bin/bash

ssh -t kongdev bash -c "export LC_ALL='en_US.UTF-8' && cd /k/kongregate/current && zeus start"
