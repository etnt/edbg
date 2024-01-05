SHELL := /bin/bash

ERLC_USE_SERVER ?= true
export ERLC_USE_SERVER

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

REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3
WGET=$(shell which wget)
CURL=$(shell which curl)

.PHONY: all old test clean
all: rebar3 compile
old: old-get-deps $(BEAM_OBJS)
test: $(TBEAM)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

clean:
	rm -f ebin/*.beam
	rm -rf _build

# -----------------------
# D E P E N D E N C I E S
# -----------------------
.PHONY: compile get-deps old_deps pp_record-dep
compile:
	./rebar3 compile

get-deps: rebar3 old_deps pp_record-dep

old-get-deps: old_deps pp_record-dep

ifeq ($(WGET),)
rebar3:
	$(CURL) -O $(REBAR3_URL) && chmod +x rebar3
else
rebar3:
	$(WGET) $(REBAR3_URL) && chmod +x rebar3
endif

old_deps:
	if [ ! -d deps ]; then \
	  mkdir deps; \
	fi

pp_record-dep:
	if [ ! -d deps/pp_record ]; then \
	  cd deps; \
	  git clone https://github.com/etnt/pp_record.git; \
	  make -C pp_record old; \
	fi

rm-deps:
	rm -rf deps rebar3

-PHONY: docs
docs:
	rebar3 ex_doc
