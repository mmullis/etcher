
ERLC_FLAGS = 

EBIN_DIR = ../ebin
ERL_SRC = $(wildcard *.erl)
ERL_HEADERS = $(wildcard *.hrl) $(wildcard ../include/*.hrl)
APP_FILE = $(wildcard *.app)
ERL_OBJ = $(ERL_SRC:%.erl=$(EBIN_DIR)/%.beam) \
		$(APP_FILE:%.app=$(EBIN_DIR)/%.app) 

$(EBIN_DIR)/%.beam: %.erl $(ERL_HEADERS)
	erlc $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.app: %.app
	cp -a $< $(EBIN_DIR)

