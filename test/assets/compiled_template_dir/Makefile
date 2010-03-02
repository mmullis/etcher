
ETCHER_COMPILER = ../../../priv/bin/etcher-compile

.PHONY: ancestors

site-page.txt.eterm: site-page.txt ancestors
	$(ETCHER_COMPILER) $<

ancestors:
	cd blog && $(MAKE)
	cd news && $(MAKE)

