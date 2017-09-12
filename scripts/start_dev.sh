#!/bin/bash
if [ -z "$AQL_NAME" ]; then
	export AQL_NAME='aql@127.0.0.1'
fi
./rebar3 shell --name=$AQL_NAME --setcookie antidote