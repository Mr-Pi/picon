#!/bin/bash
APPNAME='picon'
rebar compile
erl -name "${APPNAME}_${RANDOM}@data.mrpi" -pa ebin deps/*/ebin \
	-eval "application:start(lager)" \
	-eval "lager:set_loglevel(lager_console_backend,debug)." \
	-eval "application:start($APPNAME)."
echo -e "\e[0;33mreturn value: \e[1;31m$?\e[0m"
