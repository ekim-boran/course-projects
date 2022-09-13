
STACK=stack --allow-different-user
BUILD_OPTS=--ghc-options -O0 

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

clean: 
	$(STACK) clean

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c lib/

ghci:
	$(STACK) exec -- ghci

turnin: 
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse130/02-random-art.git

update:
	git pull upstream master
