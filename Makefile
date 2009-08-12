APPNAME=friendfs

DIRS=friendfs/src
ROOTDIR=`pwd`

## If fuserl is not install centrally look for it in a subdir this project
export ERL_COMPILE_FLAGS += -pa $(PWD)/fuserl/ebin/
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
