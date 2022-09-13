
#####################################################################################################
COURSE=cs130sp19
ASGN=05
NAME=classes
STACK=stack
BUILD_OPTS=--ghc-options -O0 
#####################################################################################################

test: clean
	$(STACK) test $(BUILD_OPTS)

bin:
	$(STACK) build $(BUILD_OPTS)

repl: 
	$(STACK) run

clean: 
	$(STACK) clean
	rm -rf tests/input/*.log

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c src/

turnin:
	git commit -a -m "turnin"
	git push origin master

ghci:
	$(STACK) exec -- ghci
