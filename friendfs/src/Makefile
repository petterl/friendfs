EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES=$(wildcard *.erl)

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.beam) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): $(INCLUDE)/friendfs.hrl

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.beam:%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

