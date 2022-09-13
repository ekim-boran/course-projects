
STACK=stack --allow-different-user
BUILD_OPTS=--ghc-options -O0 

.PHONY: clean

test: init.txt
	$(STACK)  test $(BUILD_OPTS) 

bin: init.txt
	$(STACK)  build $(BUILD_OPTS)

clean: 
	$(STACK)  clean

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


