APPNAME=friendfs

DOT=dot
RUNTIME=$(SCRIPTS)/runtime.escript

DOTFILES=$(wildcard *.dot)
PNGFILES=$(DOTFILES:%.dot=%.png)

all: $(PNGFILES)

clean:
	@for i in `cat .gitignore`; do \
		test -f $$i && (echo "rm $$i"; rm -f $$i); \
	done 
	rm -f $(PNGFILES)

## SUB TARGETS

%.png: %.dot
	@echo "Generating $@"
	$(DOT) -Tpng $< > $@

%.html: ../src/%.erl
	@echo Generating $@
	erl -noshell -eval 'edoc:file("$<",[{dir,"$(PWD)"}])' -s init stop