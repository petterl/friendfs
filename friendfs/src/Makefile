EBIN=../ebin
INCLUDE=../include

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES=$(wildcard *.erl)

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.beam) $(APP_TARGET) $(APPUP_TARGET)

YRL_SOURCES=$(wildcard *.yrl)
YRL_OBJECTS=$(YRL_SOURCES:%.yrl=%.erl) $(APP_TARGET) $(APPUP_TARGET)

all: $(YRL_OBJECTS) $(OBJECTS)

$(OBJECTS): $(INCLUDE)/friendfs.hrl

clean:
	rm -f $(YRL_OBJECTS)
	rm -f $(OBJECTS)

$(EBIN)/%.beam:%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

%.erl:%.yrl
	erl -noshell -s yecc file $< -s init stop
	make $0
