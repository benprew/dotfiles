export KONGROOT="/Users/benprew/src/kongregate"

if [[ -f $HOME/.kongpass ]]; then
    . ~/.kongpass
fi

alias ku='ssh kongdev sudo /sbin/service unicorn stop; sleep 2; ssh kongdev sudo /sbin/service unicorn start'
