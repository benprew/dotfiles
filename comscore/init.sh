# use git 2.9 on linliveanalytics
source /opt/rh/rh-git29/enable

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