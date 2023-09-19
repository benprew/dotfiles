if command -v rbenv &>/dev/null; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
    export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"
fi

if which ruby >/dev/null && which gem >/dev/null; then
    export PATH="$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

if [ -f /usr/local/share/chruby/chruby.sh ]; then
    source /usr/local/share/chruby/chruby.sh
fi
