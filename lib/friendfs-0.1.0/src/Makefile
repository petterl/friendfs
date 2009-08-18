EBIN=../ebin
INCLUDE=../include
ERL_CALL=erl_call
ERL_COMPILE_FLAGS += -pz ../../../lib/fuserl-2.0.5/ebin/ -pz ../../../lib/friendfs-0.1.0/ebin/

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

INCLUDES=$(wildcard $(INCLUDE)/*.hrl)
APPSRC=$(wildcard *.appSrc)
APP=$(APPSRC:%.appSrc=$(EBIN)/%.app)

SOURCES=$(wildcard *.erl)
YRL_SOURCES=$(wildcard *.yrl)
YRL_OBJECTS=$(YRL_SOURCES:%.yrl=$(EBIN)/%.beam)

MODULES := $(shell echo $(SOURCES:%.erl=%) | sed 's/ /,/g')

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.beam) $(YRL_OBJECTS) $(APP_TARGET) $(APPUP_TARGET)

all:$(EBIN) $(OBJECTS) $(APP)

$(EBIN):
	@mkdir $@

$(OBJECTS): $(INCLUDES)

clean:
	rm -fr $(OBJECTS) $(APP) $(EBIN)

$(EBIN)/%.beam:%.erl
	@echo "Compiling and loading $<"
	@erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
#	@echo "Copy to $(ERL_RUNTIME)/patches and load into VM if running"
#	-@cp $@ $(ERL_RUNTIME)/patches/
	-@$(shell $(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code purge [$(basename $<)]" > /dev/null 2> /dev/null)
	-@$(shell $(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code load_file [$(basename $<)]" > /dev/null 2> /dev/null)

%.erl:%.yrl
	@erl -noshell -s yecc file $< -s init stop > /dev/null

$(EBIN)/%.app:%.appSrc
	@echo "Compiling $(basename $<).app"
	$(shell sed -e 's/modules[^}]*/modules,\[$(MODULES)\]/' $< > $@)

