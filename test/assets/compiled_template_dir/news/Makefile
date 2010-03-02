
ETCHER_COMPILER = ../../../../priv/bin/etcher-compile

.PHONY: ancestors

news-page.txt.eterm: news-page.txt ancestors
	$(ETCHER_COMPILER) $<

ancestors:
	cd items && $(MAKE)

