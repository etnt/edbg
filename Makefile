SHELL := /bin/bash

ERLC=erlc
USE_COLORS ?= USE_COLORS
ERLC_FLAGS= +debug_info +compressed -D$(USE_COLORS)

BEAM=$(patsubst %.erl,%.beam,$(wildcard src/*.erl))
TBEAM=$(patsubst %.erl,%.beam,$(wildcard test/*.erl))

# To disable color: env USE_COLORS=false make
# Ugly as hell, but wtf...
ifeq "$(USE_COLORS)" "USE_COLORS"
BEAM_OBJS=$(BEAM)
else
BEAM_OBJS=src/edbg.beam src/edbg_tracer.beam src/edbg_trace_filter.beam
#BEAM_OBJS=$(filter-out *color*, $(BEAM))
endif


all: get-deps $(BEAM_OBJS)
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
get-deps: rebar3 deps pp_record-dep

rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

deps:
	if [ ! -d deps ]; then \
	  mkdir deps; \
	fi

pp_record-dep:
	if [ ! -d deps/pp_record ]; then \
	  cd deps; \
	  git clone https://github.com/etnt/pp_record.git; \
	  make -C pp_record all; \
	fi

rm-deps:
	rm -rf deps
