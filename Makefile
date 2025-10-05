suite=$(if $(SUITE), suite=$(SUITE), )

.PHONY:	all

all: compile test xref dialyze

compile:
	rebar3 compile

test:
	rebar3 eunit $(suite) skip_deps=true

xref:
	rebar3 xref

dialyze:
	rebar3 dialyzer

docs:
	rebar3 edoc

clean:
	rebar3 clean
	$(RM) doc/*
	$(RM) -rf _build/
