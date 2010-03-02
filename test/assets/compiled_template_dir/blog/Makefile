
ETCHER_COMPILER = ../../../../priv/bin/etcher-compile

.PHONY: ancestors

blog-page.txt.eterm: blog-page.txt ancestors
	$(ETCHER_COMPILER) $<

ancestors:
	cd items && $(MAKE)

