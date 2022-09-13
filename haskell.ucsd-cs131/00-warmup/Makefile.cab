
STACK=cabal
BUILD_OPTS=--ghc-options -O0


.PHONY: clean

test: init.txt
	$(STACK) v2-test $(BUILD_OPTS) --test-show-details=always

bin: init.txt
	$(STACK) v2-build $(BUILD_OPTS)

clean: 
	$(STACK) v2-clean

distclean: clean 
	rm -rf dist-newstyle 

init.txt:
	$(STACK) update > init.txt

turnin: 
	git commit -a -m "turnin"
	git push origin main 

upstream:
	git remote add upstream https://github.com/ucsd-cse131/00-warmup.git

update:
	git pull upstream main 

ghci: init.txt
	$(STACK) v2-repl $(BUILD_OPTS)


