
comma := ,
empty :=
space := $(empty) $(empty)

DOC_DIR = html
ALL_DOCS = $(DOC_DIR)/*.html $(DOC_DIR)/*.png \
		$(DOC_DIR)/*.css $(DOC_DIR)/edoc-info
INDEX = $(DOC_DIR)/index.html
ERL_SRC_DIR = ../src
ERL_SRC = $(wildcard $(ERL_SRC_DIR)/*.erl)
MOD_DOCS = $(ERL_SRC:$(ERL_SRC_DIR)/%.erl=$(DOC_DIR)/%.html)

EDOC_OPTS = '[{dir, "$(DOC_DIR)"}, {def, {version, "$(VER)"}}]'

all: $(INDEX) $(MOD_DOCS)

$(INDEX): $(ERL_SRC) 
	erl -noshell -run edoc_run application \
		"'$(APP_NAME)'" '"$(DOC_DIR)"' $(EDOC_OPTS)

$(MOD_DOCS): $(ERL_SRC)
	erl -noshell -run edoc_run files \
		'[$(subst $(space),$(comma),$(^:%="%"))]' \
		$(EDOC_OPTS)

clean:
	rm -f $(ALL_DOCS)

