NAME=parse_pdu
REBAR=./rebar

.PHONY: test

all: compile escriptize

escriptize: compile xref
	@$(REBAR) escriptize

compile: get-deps
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref skip_deps=true

get-deps:
	@$(REBAR) get-deps
