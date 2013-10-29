export KONGROOT="/Users/benprew/src/kongregate"

if [[ -f $HOME/.kongpass ]]; then
    . ~/.kongpass
fi

alias ku='ssh kongdev sudo /sbin/service unicorn stop; sleep 5; ssh kongdev sudo /sbin/service unicorn start'
alias s="open-story"

export PATH="$PATH:/usr/local/Cellar/ruby/2.0.0-p247/bin"
