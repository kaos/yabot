#!/usr/bin/sh

if [ "$#" -gt 0 ]; then
    CFG_FILE=$1
fi

if [ -f ${CFG_FILE:="yabot.config"} ]; then
    CONFIG="-config $CFG_FILE"
fi

erl -pa ebin deps/*/ebin $CONFIG -s yabot
