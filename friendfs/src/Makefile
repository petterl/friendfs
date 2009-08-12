EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES=$(wildcard *.erl)
YRL_SOURCES=$(wildcard *.yrl)
YRL_OBJECTS=$(YRL_SOURCES:%.yrl=$(EBIN)/%.beam)

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.beam) $(YRL_OBJECTS) $(APP_TARGET) $(APPUP_TARGET)


all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/friendfs.hrl

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.beam:%.erl
	@echo "Compiling and loading $<"
	@erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
#	@echo "Copy to $(ERL_RUNTIME)/patches and load into VM if running"
	@cp $@ $(ERL_RUNTIME)/patches/
	@$(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code purge [$(basename $<)]" > /dev/null
	@$(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code load_file [$(basename $<)]" > /dev/null

%.erl:%.yrl
	@erl -noshell -s yecc file $< -s init stop > /dev/null

