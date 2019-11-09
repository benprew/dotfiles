# use git 2.9 on linliveanalytics

if [[ -f /opt/rh/rh-git29/enable ]]; then  
    source /opt/rh/rh-git29/enable
fi

if [ -f /home/as_worker/as_work/analytical-solutions/etc/bashrc ]; then
    . /home/as_worker/as_work/analytical-solutions/etc/bashrc
fi

daterange () {
    startdate=$1
    enddate=$2

    curr="$startdate"
    while [[ ! "$curr" > "$enddate" ]]; do
        echo "$curr"
        curr=$( date +%Y-%m-%d --date "$curr +1 day" )
    done
}

log () {
    # It's not easy to get the PID of the child process, so for now we
    # log the current process PID.
    while read LINE; do
        echo "$(date +'%F %H:%M:%S') [$$] - $LINE"
    done
}

if [[ -n $SSH_AUTH_SOCK && $SSH_AUTH_SOCK != "/nfs/old_home/bprew/.ssh_agent_socket" ]]; then
        echo creating auth_sock link "$SSH_AUTH_SOCK"
        rm ~/.ssh_agent_socket
        ln -sf $SSH_AUTH_SOCK ~/.ssh_agent_socket
        export SSH_AUTH_SOCK=~/.ssh_agent_socket
fi
