#!/usr/bin/sh

if [ -f yabot.config ]; then
    CONFIG="-config yabot.config"
fi

erl -pa ebin deps/*/ebin ${CONFIG} -s yabot_app
