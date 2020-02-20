alias ea='$EDITOR ~/dotfiles/core/aliases.sh && . ~/dotfiles/core/aliases.sh && pushd ~/dotfiles && git commit -m "edited aliases" core/aliases.sh && git push origin master && popd'

alias ls='ls -lh'
alias sl='ls -lh'
alias e='emacsclient -n'
alias ll='ls -l'

alias flushcache='sudo killall -HUP mDNSResponder'
