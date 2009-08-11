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
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

%.erl:%.yrl
	erl -noshell -s yecc file $< -s init stop

