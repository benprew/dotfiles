alias gs 'git status'
alias gd 'git diff'
alias gco 'git checkout'
alias e 'emacsclient -n'

function f;
    find . -name "*$argv*";
end

set script_dir (dirname (status --current-filename))

# this causes extreme slowdown starting a new shell with cosmic terminal
# if status --is-interactive
#     . $script_dir/base16-ashes.fish
# end

set -U fish_greeting
