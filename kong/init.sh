export KONGROOT="/Users/benprew/src/kongregate"

if [[ -f $HOME/.kongpass ]]; then
    . ~/.kongpass
fi

alias ku='ssh kongdev sudo /sbin/service unicorn stop; sleep 5; ssh kongdev sudo /sbin/service unicorn start'
alias s="open-story"

# Gem executables
export PATH=$PATH:/usr/local/lib/ruby/gems/2.1.0/bin
alias today="yesterday today"
alias summary="kong-summary.rb"
alias bounce="skd sudo monit restart"

function y {
    yesterday | awk '{print $3}' | grep -v master | sort -u
}

function t {
    yesterday today | awk '{print $3}' | grep -v master | sort -u
}

function d {
    grep '\[DEPLOY\]' ~/.konglog
}

function skd {
    ssh -q -t kongdev $*
}

function skdc {
    skd "cd /k/kongregate/current && $*"
}

function z {
    skd "cd /k/kongregate/current && zeus $*"
}
