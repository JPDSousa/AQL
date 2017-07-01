REBAR = $(shell pwd)/rebar3
AQL = ./_build/default/lib/aql
NODE_NAME = 'aql@127.0.0.1'
NODE_DEV_NAME = 'aqldev@127.0.0.1'
COOKIE = antidote
MAIN = "aqlparser:start_shell()"
EBIN = ./_build/default/lib/*/ebin
TEST_LOGS = _build/test/logs

.PHONY: all test clean

shell:
	$(REBAR) compile
	erl -pa $(AQL)/ebin -name $(NODE_NAME) -setcookie $(COOKIE) -noshell -eval $(MAIN)

dev:
	$(REBAR) shell --name=$(NODE_DEV_NAME) --setcookie $(COOKIE)

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit --cover
	$(REBAR) cover

ct: compile
	ct_run -pa $(EBIN) -logdir $(TEST_LOGS) -dir test -erl_flags -name $(NODE_NAME) -setcookie $(COOKIE)
