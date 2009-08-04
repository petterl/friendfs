APPNAME=friendfs

DIRS = friendfs/src

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


deploy: update_rel
	escript  runtime.escript create_tar friendfs.rel

rel: update_rel
	escript  runtime.escript create_rel friendfs.rel

update_rel: friendfs.rel
	escript runtime.escript update_rel $<	
