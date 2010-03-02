
ETCHER_COMPILER = ../../../priv/bin/etcher-compile

.PHONY: ancestors clean

all: site-page.txt.eterm ancestors

site-page.txt.eterm: site-page.txt 
	$(ETCHER_COMPILER) $<

ancestors:
	cd blog && $(MAKE)
	cd news && $(MAKE)

clean:
	rm -f *.eterm
	cd blog && $(MAKE) clean
	cd news && $(MAKE) clean

