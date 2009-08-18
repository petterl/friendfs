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

clean_environment:
	@rm -f erts-$(ERTS_VSN)
	@rm -f releases/$(REL_VSN)/$(APPNAME).boot
	@rm -f $(APPNAME).rel
	@rm -f $(APPNAME).script
	@rm -rf releases
	@rm -f .make.cache
	@rm -rf pipes
	@rm -rf logs
	@rm -rf bin
	@rm -f $(APP_VSNS:%=lib/%)

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

check_environment: erts-$(ERTS_VSN) $(APP_VSNS:%=lib/%) releases/$(REL_VSN)/start.boot releases/$(REL_VSN)/sys.config releases/start_erl.data bin pipes log


releases/$(REL_VSN)/%.boot: $(APPNAME).script releases/$(REL_VSN)
	$(ERL) $(DIRS:%/src=-pz %/ebin) -noshell -s systools script2boot $(basename $<) -s init stop
	mv $(subst script,boot,$<) $@

%.script: %.rel
	$(ERL) $(DIRS:%/src=-pz %/ebin) -noshell -s systools make_script $(basename $<) -s init stop

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

releases/start_erl.data: $(APPNAME).relSrc
	echo "$(ERTS_VSN) $(REL_VSN)" > $@

bin:
	cp -r  $(shell escript $(RUNTIME) erl_root)/bin .
	sed -e 's:ROOTDIR=[^\\n]*:BINDIR=`pwd`/`dirname $$0`\nROOTDIR=`dirname $$BINDIR`:' \
	    -e 's:-daemon [^$$]*:-daemon $$ROOTDIR/pipes/ :' \
	    -e 's:$$START_ERL_DATA:$$START_ERL_DATA -pa $$ROOTDIR/patches/ -sname $(APPNAME) -setcookie $(APPNAME):' $@/start > /tmp/start.erl
	mv -f /tmp/start.erl $@/start
	chmod +x $@/start
#	RootStr = re:replace(Str,"ROOTDIR=[^\\n]*","BINDIR=`pwd`/`dirname $0`\nROOTDIR=`dirname $BINDIR`",[{return,list}]),
#    DaemonStr = re:replace(RootStr,"-daemon [^$]*","-daemon $ROOTDIR/pipes/ ",
#			   [{return,list}]),
#    NameStr = re:replace(DaemonStr,"\\$START_ERL_DATA",
#			 "$START_ERL_DATA -pa $ROOTDIR/patches/ -sname "++Relname
#			 ++" -setcookie "++Relname,
#			 [{return,list}]),

pipes log:
	mkdir -p $@
