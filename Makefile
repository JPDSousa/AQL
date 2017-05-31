REBAR = $(shell pwd)/rebar3

shell:
	$(REBAR) shell --name='aql@127.0.0.1' --setcookie antidote

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit
