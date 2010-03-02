
ETCHER_COMPILER = ../../../../priv/bin/etcher-compile

.PHONY: ancestors clean

all: news-page.txt.eterm ancestors

news-page.txt.eterm: news-page.txt 
	$(ETCHER_COMPILER) $<

ancestors:
	cd items && $(MAKE)

clean:
	rm -f *.eterm
	cd items && $(MAKE) clean

