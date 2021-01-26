export PATH=$PATH:~/.local/bin

if command -v pyenv >/dev/null; then
    export PATH=$(pyenv root)/shims:$PATH
fi

# if command -v pyenv-virtualenv-init > /dev/null; then
#     eval "$(pyenv virtualenv-init -)";
# fi
