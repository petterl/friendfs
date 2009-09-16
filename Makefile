-include .make.cache
-include VSN.mk

APPNAME=friendfs

SCRIPTS=./scripts/
RUNTIME=$(SCRIPTS)/runtime.escript

DIRS=lib/fuserl-2.0.5/src friendfs/src
ROOTDIR=`pwd`

APPS:=$(shell cat friendfs.relSrc | sed 's/[\[\t{ ]*\([^,]*\).*/\1/' | grep -v release | grep -v erts | grep -v friendfs | awk '{ printf "%s ", $$0 }')
ERTS_VSN ?= $(shell escript $(RUNTIME) get_erts_vsn)
ERL_CALL=erl_call
ERL=erl -boot start_clean $(ERL_COMPILE_FLAGS)
export ERL_COOKIE=friendfs
export ERL_SNAME=friendfs
export ERL_COMPILE_FLAGS+= -pa $(PWD)/lib/friendfs-$(FRIENDFS_VSN)/ebin $(foreach app,$(APP_VSNS), -pa $(PWD)/lib/$(app)/ebin)
ERL_RUNTIME=$(PWD)/rts/

all: setup_libs subdirs setup_release

subdirs:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE)); \
	done

test:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE) test); \
	done

clean: clean_beam clean_release clean_docs
	rm -f .make.cache
	rm -f erl_crash.dump
	find . -name '*~' -exec rm {} \;

clean_beam:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE) clean); \
	done

clean_release:
	rm -f erts-$(ERTS_VSN)
	rm -f $(APPNAME).rel
	rm -f $(APPNAME).script
	rm -rf releases
	rm -rf pipes
	rm -rf log
	rm -rf bin
	rm -rf patches
	rm -f lib/friendfs-*
	-rm -f $(APP_VSNS:%=lib/%)

clean_docs:
	$(foreach d,$(DIRS),rm -f $d/../doc/*.html $d/../doc/edoc-info $d/../doc/stylesheet.css $d/../doc/erlang.png 2> /dev/null & )

docs:subdirs
	$(ERL) $(foreach dir,$(DIRS:%/src=%/ebin),-pa $(dir) ) -noshell -eval "edoc:application($(APPNAME))" -s init stop

setup_libs: lib lib/friendfs-$(FRIENDFS_VSN) $(APP_VSNS:%=lib/%)

setup_release: erts-$(ERTS_VSN) setup_libs releases/$(REL_VSN) releases/$(REL_VSN)/start.boot releases/$(REL_VSN)/sys.config releases/start_erl.data bin pipes log patches

## SUB TARGETS

%.rel: %.relSrc
	@echo "Updating $@"
	cat $< | sed -e 's/"friendfs",""/"friendfs","$(REL_VSN)"/g' \
		-e 's/friendfs,""/friendfs,"$(FRIENDFS_VSN)"/g' \
		-e 's/erts,""/erts,"$(ERTS_VSN)"/g' \
		$(foreach app,$(APP_VSNS), -e 's/\($(firstword $(subst -, ,$(app)))\),""/\1,"$(word 2,$(subst -, ,$(app)))"/g' ) > $@

.make.cache: $(APPNAME).relSrc
	@echo APP_VSNS=$(foreach app, $(APPS),$(app)-$(shell escript $(RUNTIME) get_app_vsn $(app))) > $@
	@echo ERTS_VSN=$(shell escript $(RUNTIME) get_erts_vsn) >> $@

releases/$(REL_VSN)/%.boot: $(APPNAME).script $(APPNAME).rel 
	$(ERL) $(DIRS:%/src=-pz %/ebin) -noshell -s systools script2boot $(basename $<) -s init stop
	mv $(subst script,boot,$<) $@


%.script: %.rel
	$(ERL) $(DIRS:%/src=-pz %/ebin) -noshell -s systools make_script $(basename $<) -s init stop

.PHONY: force
force: ;

friendfs/%: force 
	cd friendfs && $(MAKE) src/$(notdir $@)

erts-%:
	ln -s $(shell escript $(RUNTIME) erl_root)$@

lib/friendfs-%:
	(cd lib && ln -s ../friendfs $(notdir $@))

$(APP_VSNS:%=lib/%):
	(cd lib && ln -s $(shell escript $(RUNTIME) erl_root)$@)


releases/$(REL_VSN):
	mkdir -p $@

releases/$(REL_VSN)/sys.config: releases/$(REL_VSN) $(APPNAME).rel
	echo "[]." > $@

releases/start_erl.data: $(APPNAME).rel
	echo "$(ERTS_VSN) $(REL_VSN)" > $@

bin:
	cp -r  $(shell escript $(RUNTIME) erl_root)/bin .
	cp scripts/start_friendfs bin/
	chmod +x $@/start_friendfs

pipes log patches lib:
	mkdir -p $@
