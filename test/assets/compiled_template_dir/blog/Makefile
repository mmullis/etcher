
ETCHER_COMPILER = ../../../../priv/bin/etcher-compile

.PHONY: ancestors clean

all: blog-page.txt.eterm ancestors

blog-page.txt.eterm: blog-page.txt 
	$(ETCHER_COMPILER) $<

ancestors:
	cd items && $(MAKE)

clean:
	rm -f *.eterm
	cd items && $(MAKE) clean

