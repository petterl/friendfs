EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

INCLUDES=$(wildcard $(INCLUDE)/*.hrl)
APPSRC=$(wildcard *.appSrc)
APP=$(APPSRC:%.appSrc=$(EBIN)/%.app)

SOURCES=$(wildcard *.erl)
YRL_SOURCES=$(wildcard *.yrl)
YRL_OBJECTS=$(YRL_SOURCES:%.yrl=$(EBIN)/%.beam)

MODULES := $(shell echo $(SOURCES:%.erl=%) | sed 's/ /,/g')	

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.beam) $(YRL_OBJECTS) $(APP_TARGET) $(APPUP_TARGET)


all:$(OBJECTS) $(APP)


$(OBJECTS): $(INCLUDES)

clean:
	rm -f $(OBJECTS) $(APP)

$(EBIN)/%.beam:%.erl
	@echo "Compiling and loading $<"
	@erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
#	@echo "Copy to $(ERL_RUNTIME)/patches and load into VM if running"
#	-@cp $@ $(ERL_RUNTIME)/patches/
	-@$(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code purge [$(basename $<)]" > /dev/null
	-@$(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code load_file [$(basename $<)]" > /dev/null

%.erl:%.yrl
	@erl -noshell -s yecc file $< -s init stop > /dev/null

$(EBIN)/%.app:%.appSrc
	@echo "Compiling $(basename $<).app"
	$(shell sed -e 's/modules[^}]*/modules,\[$(MODULES)\]/' $< > $@)

