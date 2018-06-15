set -x PATH $PATH /usr/local/lib/ruby/gems/2.1.0/bin
set -x PATH $PATH /Applications/SnowSQL.app/Contents/MacOS/

alias gs 'git status'
alias gd 'git diff'
alias dc 'docker-compose'
alias dcrr 'docker-compose run rails'
alias restart 'dc down; and rm -f tmp/pids/server.pid; and dc up'

function stream
  aws s3 cp $argv - | gzcat
end
