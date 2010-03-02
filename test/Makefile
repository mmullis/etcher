
.PHONY: src test assets

test: src assets
	./run_tests 

src:
	cd src && $(MAKE)

assets:
	cd assets && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd assets && $(MAKE) clean
	rm -f tmp/*

