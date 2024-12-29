REBAR=rebar3
EUNIT_ARGS=

all: compile

compile:
	$(REBAR) compile

test:
	$(REBAR) eunit --verbose $(EUNIT_ARGS)

run:
	$(REBAR) as test shell --name "ja_erl_utils" --apps sync

docs:
	$(REBAR) as docs edoc


.PHONY: all compile test run docs
