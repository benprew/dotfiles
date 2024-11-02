
alias gs 'git status'
alias gd 'git diff'
alias gco 'git checkout'
alias e 'emacsclient -n'

function f;
    find . -name "*$argv*";
end

set script_dir (dirname (status --current-filename))
. $script_dir/base16-solarized-dark.fish
