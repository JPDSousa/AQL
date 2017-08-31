#!/bin/bash
if [ -z "$AQL_DEV" ]; then
	export AQL_DEV='aqldev@127.0.0.1'
fi
./rebar3 shell --name=$AQL_DEV --setcookie antidote