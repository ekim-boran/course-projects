#####################################################################################################
COURSE=cs130wi21
ASGN=01
NAME=haskell
STACK=stack
#####################################################################################################

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

test: clean
	$(STACK) test --allow-different-user

bin:
	$(STACK) build --allow-different-user

clean:
	$(STACK) clean --allow-different-user

ghci:
	$(STACK) exec --allow-different-user -- ghci

distclean: clean
	rm -rf .stack-work

turnin:
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse130/01-haskell.git

update:
	git pull upstream master
