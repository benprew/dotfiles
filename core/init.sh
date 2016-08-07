unset PROMPT_COMMAND
export EDITOR='emacsclient'
export PATH=/usr/local/bin:/usr/local/sbin:$PATH


__dotfiles_status ()
{
    if [[ `ruby --version | grep jruby` != "" ]]; then
        return
    fi

    diffs=`cd ~/dotfiles && rake -s show_differences_stat`
    if [[ $diffs != "" ]]; then
        printf "%s " $diffs
    fi
}

. ~/dotfiles/core/aliases.sh
