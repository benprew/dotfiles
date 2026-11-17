#!/bin/bash

# Full install instructions:
# https://zulip.readthedocs.io/en/latest/development/setup-recommended.html

# from https://docs.docker.com/engine/install/fedora/
sudo dnf install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

sudo systemctl enable --now docker
