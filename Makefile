
export APP_NAME = etcher
export VER = 0.2.1

.PHONY: src doc clean

src: 
	cd src && $(MAKE)

test:
	cd test && $(MAKE) test

doc:
	cd doc && $(MAKE)

clean:
	cd src && $(MAKE) clean

pristine: clean
	cd doc && $(MAKE) clean

