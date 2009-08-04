APPNAME=friendfs

DIRS = src

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
