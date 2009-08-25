EBIN=../ebin
INCLUDE=../include
TEST_DIR=../tests
DBG_DIR=dbg/
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
TEST_SOURCE =$(wildcard $(TEST_DIR)/*.hrl) 
TEST_OBJECTS=$(TEST_SOURCE:$(TEST_DIR)/%.hrl=$(DBG_DIR)/%.beam)

all:$(EBIN) $(OBJECTS) $(APP)

$(EBIN):
	@mkdir $@

$(DBG_DIR):
	@mkdir $@

$(OBJECTS): $(INCLUDES)

clean:
	rm -fr $(OBJECTS) $(APP) $(EBIN) $(DBG_DIR)

$(EBIN)/%.beam:%.erl
	@echo "Compiling and loading $<"
	@erlc -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
#	@echo "Copy to $(ERL_RUNTIME)/patches and load into VM if running"
#	-@cp $@ $(ERL_RUNTIME)/patches/
	-$(shell $(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code purge [$(basename $<)]" > /tmp/log 2> /dev/null )
	-$(shell $(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code delete [$(basename $<)]" >> /tmp/log 2> /dev/null )
	-$(shell $(ERL_CALL) -c $(ERL_COOKIE) -sname $(ERL_SNAME) -a \
		"code load_file [$(basename $<)]"  >> /tmp/log 2> /dev/null )

%.erl:%.yrl
	@erl -noshell -s yecc file $< -s init stop > /dev/null

$(EBIN)/%.app:%.appSrc
	@echo "Compiling $(basename $<).app"
	$(shell sed -e 's/modules[^}]*/modules,\[$(MODULES)\]/' $< > $@)

$(DBG_DIR)/%.beam:%.erl $(TEST_DIR)/%.hrl
	@erlc -I $(TEST_DIR) -W $(ERL_COMPILE_FLAGS) -DTEST -o $(DBG_DIR)/ $<
	@echo "Testing $(basename $(notdir $@))"
	@erl -noshell -pa dbg/ -s eunit test $(basename $<) -s init stop

test:all $(DBG_DIR) $(TEST_OBJECTS)
