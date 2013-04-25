ERL       ?= erl
ERLC      ?= $(ERL)c
APP       := yabot

# Erlang Rebar downloading
# see: https://groups.google.com/forum/?fromgroups=#!topic/erlang-programming/U0JJ3SeUv5Y
REBAR := $(shell (type rebar 2>/dev/null || echo ./rebar) | tail -1 | awk '{ print $$NF }')
REBAR_DEPS := $(shell which rebar || echo ../../rebar)
REBAR_URL := https://github.com/rebar/rebar/wiki/rebar

# Default target - update sources and call all compile rules in succession
.PHONY: all
all: get-deps update-deps compile


./rebar:
	$(ERL) -noshell -s inets -s ssl \
	  -eval '{ok, saved_to_file} = httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
	  -s init stop
	chmod +x ./rebar

get-deps: $(REBAR)
	$(REBAR) get-deps

update-deps: $(REBAR)
	$(REBAR) update-deps

compile: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean
