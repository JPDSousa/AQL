REBAR = $(shell pwd)/rebar3
AQL = ./_build/default/lib/aql
NODE_NAME = 'aql@127.0.0.1'
NODE_DEV_NAME = 'aqldev@127.0.0.1'
COOKIE = antidote
MAIN = "aqlparser:start_shell()"
EBIN = ./_build/default/lib/*/ebin
TEST_LOGS = _build/test/logs
ANTIDOTE = antidote/_build/default/rel/antidote
SCRIPTS = ./scripts

.PHONY: all test clean antidote

shell: compile
	chmod u+x $(SCRIPTS)/start_shell.sh
	$(SCRIPTS)/start_shell.sh

dev_public:
	chmod u+x $(SCRIPTS)/start_dev_public.sh
	$(SCRIPTS)/start_dev_public.sh

shell_public:
	chmod u+x $(SCRIPTS)/start_shell_public.sh
	$(SCRIPTS)/start_shell_public.sh

dev:
	chmod +x $(SCRIPTS)/start_dev.sh
	$(SCRIPTS)/start_dev.sh

compile:
	$(REBAR) compile
	mkdir -p _build/test/logs

test:
	$(REBAR) eunit --cover
	$(REBAR) cover

ct: compile
	ct_run -pa $(EBIN) -logdir $(TEST_LOGS) -dir test -include include -erl_flags -name $(NODE_NAME) -setcookie $(COOKIE)

antidote:
	./$(ANTIDOTE)/bin/env start && sleep 10 && tail -f $(ANTIDOTE)/log/console.log &
