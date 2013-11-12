#!/bin/bash
if [ $ENVIRONMENT = 'erlang' ]
then
	echo "stop environment"

	unset APPNAME
	unset AUTOMODULE
	unset ENVIRONMENT

	unalias rs
	unalias vim
	unalias end
	
	for i in "C-c" "a" "C-c" "C-c" "C-c" "C-d" "C-c" "C-c"
	do
		tmux send-keys -t 1 $i
		sleep 0.25
	done
else
	MAXWIDTH=`tput cols`
	RWIDTH=$((MAXWIDTH/3))

	tmux split-window -h 
	tmux resize-pane -x $RWIDTH
	tmux split-window 'while true; do rebar get-deps && hg status && date && sleep 30; done'
	tmux resize-pane -y 13
	tmux select-pane -L

	tmux send-keys -t 1 'while true; do ./start-dev.sh; done'

	unset MAXWIDTH
	unset RWIDTH

	export APPNAME='picon'
	export AUTOMODULE='picon_mainServer'
	export ENVIRONMENT='erlang'
	
	alias rs="tmux send-keys -t 1 'halt().' C-m"
	alias vim="vim +\"map %R :!tmux send-keys -t 1 'halt().' C-m<CR><CR>\" +\"imap %R <ESC>%Ra\""
	alias end=". ./environment.sh"

	vim +"map %R :!tmux send-keys -t 1 'halt().' C-m<CR><CR>" +"imap %R <ESC>%Ra"
fi
