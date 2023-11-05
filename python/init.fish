add_path $HOME/.pyenv/bin
add_path (python3 -m site --user-base)/bin
status --is-interactive; and command -vq pyenv; and pyenv init - | source
status --is-interactive; and command -vq pyenv; and pyenv virtualenv-init - | source
