APPNAME=friendfs

DIRS = friendfs/src

export ERL_COMPILE_FLAGS= -pa $(PWD)/fuserl/ebin

all: subdirs

fuserl/README: .gitmodules
	git submodule update --init fuserl

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

deploy: all rel
	escript  runtime.escript create_tar friendfs.rel
	cp friendfs.tar.gz rts
	(cd rts && tar xvzf friendfs.tar.gz)

rel: update_rel rts
	escript  runtime.escript create_rel friendfs.rel

update_rel: friendfs.rel
	escript runtime.escript update_rel $<	
