#!/bin/bash

# set -x

WORK=25
PAUSE=5
INTERACTIVE=true
MUTE=false

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
SCRIPT=$(basename "$0")


show_help() {
    cat <<-END
usage: $SCRIPT [-s] [-m] [-w m] [-b m] [-h]
       -s: simple output. Intended for use in scripts
           When enabled, potato outputs one line for each minute, and doesn't print the bell character
           (ascii 007)
       -m: mute -- don't play sounds when work/break is over
       -w m: let work periods last m minutes (default is 25)
       -b m: let break periods last m minutes (default is 5)
       -h: print this message
END
}

play_notification() {
    for f in {1..3}; do
        echo "$f"
        afplay "/System/Library/Sounds/Submarine.aiff"
    done
}

while getopts :sw:b:mh opt; do
    case "$opt" in
        s)
            INTERACTIVE=false
            ;;
        m)
            MUTE=true
            ;;
        w)
            WORK=$OPTARG
            ;;
        b)
            PAUSE=$OPTARG
            ;;
        h|\?)
            show_help
            exit 1
            ;;
    esac
done

time_left="%im left of %s "

if $INTERACTIVE; then
    time_left="\r$time_left"
else
    time_left="$time_left\n"
fi

while true
do
    for ((i=$WORK; i>0; i--))
    do
        printf "$time_left" $i "work"
        sleep 60
    done

    ! $MUTE && play_notification
    if $INTERACTIVE; then
        read -r -d '' -t 1
        echo -e "\a"
        echo "Work over"
        read -r
    fi

    for ((i=$PAUSE; i>0; i--))
    do
        printf "$time_left" $i "pause"

        sleep 60
    done

    ! $MUTE && play_notification
    if $INTERACTIVE; then
        read -r -d '' -t 0.001
        echo -e "\a"
        echo "Pause over"
        read -r
    fi
done
