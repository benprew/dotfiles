if test -e $HOME/.pyenv/bin
    set -g fish_user_paths $fish_user_paths $HOME/.pyenv/bin
end
status --is-interactive; and command -vq pyenv; and pyenv init - | source
status --is-interactive; and command -vq pyenv; and pyenv virtualenv-init - | source
