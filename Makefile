APPNAME=friendfs

SCRIPTS=./scripts/
RUNTIME=$(SCRIPTS)/runtime.escript

ERL_ROOT=$(shell escript $(RUNTIME) erl_root)
ERTS_VSN=$(shell escript $(RUNTIME) get_erts_vsn)
REL_APPS=$(shell escript $(RUNTIME) get_apps)
DIRS=lib/fuserl-2.0.5/src lib/friendfs-0.1.0/src
ROOTDIR=`pwd`
APPS=$(wildcard lib/*/ebin/*.app)

## If fuserl is not install centrally look for it in a subdir this project
export ERL_COMPILE_FLAGS += -pa $(PWD)/lib/fuserl-2.0.5/ebin/ -pa $(PWD)/lib/friendfs-0.1.0/ebin/
export ERL_COOKIE=friendfs
export ERL_SNAME=friendfs
export ERL_RUNTIME=$(PWD)/rts/
export ERL_CALL=erl_call

all: subdirs

subdirs:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE)); \
	done

clean:
	@for d in $(DIRS); do \
		(cd $$d; $(MAKE) clean); \
	done

docs:
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [])" -s init stop

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

check_environment: friendfs.boot erts-$(ERTS_VSN)
	@echo $(APPS)

%.boot: %.script
	@erl $(ERL_COMPILE_FLAGS) -noshell -s systools script2boot $(basename $<) -s init stop

%.script: %.rel
	@erl $(ERL_COMPILE_FLAGS) -noshell -s systools make_script $(basename $<) -s init stop

%.rel: %.relSrc
	@echo "Updating $@"
#	cp $< $@
#	$(foreach app,$(shell cat $< | sed 's/[\[{ ]*\([^,]*\).*/\1/' | grep -v release | grep -v erts), \
		$(shell sed -e 's/$(app),""/$(app),"$(shell escript $(RUNTIME) get_app_vsn $(app))"/' $@ > $@))
	@escript $(RUNTIME) update_rel $<

erts-$(ERTS_VSN):
	ln -s $(ERL_ROOT)$@