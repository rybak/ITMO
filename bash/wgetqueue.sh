#!/bin/bash

shopt -s nullglob
function dpid {
    cat "$DAEMON_PID_FILE"
}

function print_dl {
    echo "URL $1 > $2"
}

function print_log {
    local pid=$(dpid)
    echo -n `date -Iseconds` >> "$ELOG"
    echo -n " $$ " >> "$ELOG"
    echo "PID = $pid" >> "$ELOG"
    local MESSAGE=$1
    local URL=$2
    local FILENAME=$3
    echo -e "$1\n\t" >> "$ELOG"
    echo -e "$(print_dl "$URL" "$FILENAME")" >> "$ELOG"
}

function my_notify {
    notify-send -i "$ICON" -u $1 "$TITLE" "$2"
}

function daemon {
    DAEMONDIR=$(pwd -P)
    echo "$DAEMONDIR" > "$DAEMON_PWD_FILE"
    trap "" SIGHUP
    while true
    do
        for r in "$QPATH/requests"/*$SUFREQ
        do
            echo "r = $r" >> "$ELOG"
            local url=$(cat "$r")
            local req="${r%.*}"
            rm "$r"
            local name
            local name_arg
            if [[ -f "$req$SUFNAME" ]];
            then
                name=$(cat "$req$SUFNAME")
                name_arg="--output-document=$name"
                rm "$req$SUFNAME"
            else
                name_arg=
                name=
            fi
            # my_notify normal "Started download: $(print_dl $url $name)"
            echo "$name : $url" > "$LAST"
            print_log "Download started" "$url" "$name"
            if wget $name_arg --continue "$url" 2>> "$WGETLOG";
            then
                # my_notify normal "Download complete: $(print_dl $url $name)"
                print_log "Download complete" "$url" "$name"
                rm "$LAST"
            else
                my_notify critical "Error while downloading: $(print_dl $url $name)"
                print_log "Error while downloading" "$url" "$name"
            fi
        done
        sleep $DAEMON_PERIOD
    done
}

function usage {
    echo "Usage"
    echo "./wgetqueue.sh [Flag] | [--dry] [[-n NAME] URL]..."
    echo "URL"
    echo -e "\tIf URL is given, wgetqueue.sh adds URL to queue."
    echo "Flags"
    echo -e "\t-c\n\t\tCheck if daemon is running."
    echo -e "\t-k\n\t\tKill daemon."
    echo -e "\t-h\n\t\tPrint this help."
    echo -e "\t-n NAME\n\t\tUse NAME as a filename for the next URL."
    echo -e "\t--dry\n\t\tMake a dry run, print parsed requests and exit."
    echo -e "\nLogs :\n\t$ELOG\n\t$WGETLOG\n\t$LAST"
    echo ""
}

function show_last {
    if [[ -f "$LAST" ]];
    then
        echo "$1"
        cat "$LAST"
    else
        echo "Seems that no current downloads are running."
    fi
}

QPATH=$HOME/.nyaqueue
DPATH="$PWD"
TITLE=wgetqueue
ICON=$QPATH/icon.png

DAEMON_PID_FILE=$QPATH/daemonrunning
DAEMON_PWD_FILE=$QPATH/daemonpwd
REQUESTS=$QPATH/requests
ATOM=$QPATH/atom
DAEMON_PERIOD=15s

ELOG=$QPATH/error.log
WGETLOG=$QPATH/get.log
LAST=$QPATH/last.log

SUFNAME=.name
SUFREQ=.req

if [[ $# -eq 0 ]] || [[ $1 == "-h" ]];
then
    usage
    exit
fi

mkdir -p "$QPATH"
mkdir -p "$REQUESTS"
mkdir -p "$ATOM"

if [[ $1 == "-c" ]];
then
    if [[ -f "$DAEMON_PID_FILE" ]];
    then
        pid=$(dpid)
        echo "Daemon is running: PID = $pid"
        show_last "Download in progress :"
    else
        echo "Daemon is not running."
        show_last "Unfinished download was interrupted:"
    fi
    exit
fi

if [[ $1 == "-k" ]];
then
    if [[ -f "$DAEMON_PID_FILE" ]];
    then
        # TODO make a check if there is a current download // file $LAST exists
        show_last "Unfinished download was interrupted:" >> "$ELOG"
        pid=$(dpid)
        echo "Killing daemon: PID = $pid"
        kill -9 "$pid"
        rm -f "$DAEMON_PID_FILE"
    else
        echo "Daemon is not running."
    fi
    exit
fi

if [[ $1 == "-d" ]];
then
    if [[ -f "$DAEMON_PID_FILE" ]];
    then
        pid=$(dpid)
        echo "Error: Daemon is already running: PID = $pid"
        exit 255
    fi
    daemon 1> '/dev/null' 2>&1 & disown
    echo "$!" > "$DAEMON_PID_FILE"
    pid=$(dpid)
    echo -e "Daemon started: PID = $pid"
    exit
fi

if [[ "$1" == "--dry" ]];
then
    DRY=true
    shift
else
    DRY=false
fi

while (( "$#" ));
do
    name=
    if [[ "$1" == "-n" ]];
    then
        if [[ $# -lt 3 ]];
        then
            echo "Wrong arguments: $@"
            usage
            exit 255
        fi
        shift
        name="$DPATH/$1"
        shift
    fi
    r=`mktemp -u --tmpdir="$ATOM"`
    echo "$1" > "$r$SUFREQ"
    if [[ -n "$name" ]];
    then
        echo "$name" > "$r$SUFNAME"
    fi
    if [[ "$DRY" == true ]];
    then
        echo "Request:"
        ls "$r"* | xargs cat
        rm "$r"*
    else
        r=${r##*/}
        mv "$ATOM/$r"* "$REQUESTS"
    fi
    shift
done

if [[ ! -f "$DAEMON_PID_FILE" ]]; then
    $0 -d 0<&- & disown
    sleep 1
    exit
fi

