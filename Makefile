SHELL := /bin/bash

ERLC=erlc
ERLC_FLAGS= +debug_info +compressed

BEAM=$(patsubst %.erl,%.beam,$(wildcard src/*.erl))
TBEAM=$(patsubst %.erl,%.beam,$(wildcard test/*.erl))


all: $(BEAM)
test: $(TBEAM)
.PHONY: all test

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

clean:
	rm -f ebin/*.beam
