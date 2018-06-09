set -x PATH $PATH /usr/local/lib/ruby/gems/2.1.0/bin
set -x PATH $PATH /Applications/SnowSQL.app/Contents/MacOS/

set -x HOMEBREW_GITHUB_API_TOKEN 8ea2c7459f0e5e60f322590925a7f6d6037d674f

alias gs 'git status'
alias gd 'git diff'
alias dc 'docker-compose'
alias dcrr 'docker-compose run rails'

function stream
  aws s3 cp $argv - | gzcat
end
