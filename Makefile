-include .make.cache

APPNAME=friendfs

SCRIPTS=./scripts/
RUNTIME=$(SCRIPTS)/runtime.escript

DIRS=lib/fuserl-2.0.5/src lib/friendfs-0.1.0/src
ROOTDIR=`pwd`
APPS:=$(shell cat friendfs.relSrc | sed 's/[\[{ ]*\([^,]*\).*/\1/' | grep -v release | grep -v erts | awk '{ printf "%s ", $$0 }')
ERTS_VSN=$(shell escript $(RUNTIME) get_erts_vsn)
ERL_CALL=erl_call
ERL=erl -boot start_clean $(ERL_COMPILE_FLAGS)
ERL_COOKIE=friendfs
ERL_SNAME=friendfs
ERL_RUNTIME=$(PWD)/rts/

all: subdirs

subdirs:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE)); \
	done

clean:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE) clean); \
	done
	@rm -f $(APP_VSNS:%=lib/%)
	@rm -f erts-$(ERTS_VSN)
	@rm -f releases/$(REL_VSN)/$(APPNAME).boot
	@rm -f $(APPNAME).rel
	@rm -f $(APPNAME).script
	@rm -rf releases
	@rm -f .app.cache

docs:
	$(ERL) -noshell -eval "edoc:application($(APPNAME), \".\", [])" -s init stop

rts:
	mkdir rts

install: all rel
	escript  runtime.escript create_tar friendfs.rel
	cp friendfs.tar.gz rts
	(cd rts && tar xvzf friendfs.tar.gz)

rel: update_rel rts
	escript  runtime.escript create_rel friendfs.rel

update_rel: friendfs.rel
	escript runtime.escript update_rel $<

check_environment: erts-$(ERTS_VSN) $(APP_VSNS:%=lib/%) releases/$(REL_VSN)/friendfs.boot releases/$(REL_VSN)/sys.config


releases/$(REL_VSN)/%.boot: %.script releases/$(REL_VSN)
	@$(ERL) -noshell -s systools script2boot $(basename $<) -s init stop
	mv $(subst script,boot,$<) $@

%.script: %.rel
	@$(ERL) -noshell -s systools make_script $(basename $<) -s init stop

%.rel: %.relSrc
	@echo "Updating $@"
	cat $< | sed -e 's/erts,""/erts,"$(ERTS_VSN)"/g' $(foreach app,$(APP_VSNS), -e 's/\($(firstword $(subst -, ,$(app)))\),""/\1,"$(word 2,$(subst -, ,$(app)))"/g' ) > $@

.make.cache: $(APPNAME).relSrc
	@echo APP_VSNS=$(foreach app, $(APPS),$(app)-$(shell escript $(RUNTIME) get_app_vsn $(app))) > $@
	@echo REL_VSN=$(shell cat friendfs.relSrc | grep '"$(APPNAME)"' | sed 's/.*$(APPNAME)","\([^"]*\).*/\1/') >> $@

erts-$(ERTS_VSN):
	ln -s $(shell escript $(RUNTIME) erl_root)$@

$(APP_VSNS:%=lib/%):
	(cd lib && ln -s $(shell escript $(RUNTIME) erl_root)$@)

releases/$(REL_VSN): $(APPNAME).relSrc
	mkdir -p $@

releases/$(REL_VSN)/sys.config: releases/$(REL_VSN) $(APPNAME).relSrc
	echo "[]." > $@
