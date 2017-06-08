REBAR = $(shell pwd)/rebar3
EBIN = ./_build/default/lib/aql/ebin
NODE_NAME = 'aql@127.0.0.1'
COOKIE = antidote
MAIN = "aqlparser:start_shell()"

shell:
	erl -pa $(EBIN) -name=$(NODE_NAME) -setcookie $(COOKIE) -noshell -eval $(MAIN)

dev:
	$(REBAR) shell --name=$(NODE_NAME) --setcookie $(COOKIE)

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit
