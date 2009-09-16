APPNAME=friendfs

DOT=dot
RUNTIME=$(SCRIPTS)/runtime.escript

DOTFILES=read_sequence.dot
PNGFILES=$(DOTFILES:%.dot=%.png)

all: $(PNGFILES)

clean:
	rm -f $(PNGFILES)

## SUB TARGETS

%.png: %.dot
	@echo "Generating $@"
	$(DOT) -Tpng $< > $@
