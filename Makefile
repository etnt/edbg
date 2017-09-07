SHELL := /bin/bash

ERLC=erlc
ERLC_FLAGS= +debug_info +compressed

BEAM=$(patsubst %.erl,%.beam,$(wildcard src/*.erl))
TBEAM=$(patsubst %.erl,%.beam,$(wildcard test/*.erl))


all: get-deps $(BEAM)
test: $(TBEAM)
.PHONY: all test

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

clean:
	rm -f ebin/*.beam

# -----------------------
# D E P E N D E N C I E S
# -----------------------
.PHONY: get-deps deps pp_record-dep
get-deps: deps pp_record-dep

deps:
	if [ ! -d deps ]; then \
	  mkdir deps; \
	fi

pp_record-dep:
	if [ ! -d deps/pp_record ]; then \
	  cd deps; \
	  git clone https://github.com/bet365/pp_record.git; \
	  make -C pp_record all; \
	fi

rm-deps:
	rm -rf deps
