alias ea='$EDITOR ~/dotfiles/core/aliases.sh && . ~/dotfiles/core/aliases.sh && pushd ~/dotfiles && git commit -m "edited aliases" core/aliases.sh && git push origin master && popd'

alias ls='ls -lh'
alias sl='ls -lh'
alias e='emacsclient-nt.sh'
alias es='emacsclient-nt-sudo.sh'
alias et='emacsclient-t.sh'
alias ll='ls -l'
alias vi='emacsclient-nt.sh'
alias vim='emacsclient-nt.sh'

alias flushcache='sudo killall -HUP mDNSResponder'
