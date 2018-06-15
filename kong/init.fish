set -x PATH $PATH /usr/local/lib/ruby/gems/2.1.0/bin
set -x PATH $PATH /Applications/SnowSQL.app/Contents/MacOS/

alias gs 'git status'
alias gd 'git diff'
alias dc 'docker-compose'
alias dcrr 'docker-compose run rails'

function stream
  aws s3 cp $argv - | gzcat
end
