######################################################
ORG=ucsd-cse131
ASGN=01
COMPILER=adder
EXT=adder
BUILD_OPTS=--ghc-options -O0 
######################################################
REPL=stack repl --allow-different-user
CLEAN=stack clean --allow-different-user
BUILD=stack build --allow-different-user $(BUILD_OPTS)
TEST=stack test --allow-different-user $(BUILD_OPTS)
EXEC=stack exec --allow-different-user
UPDATE=stack update
######################################################

COMPILEREXEC=$(EXEC) -- $(COMPILER)

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=elf64
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho64
else
ifeq ($(UNAME), CYGWIN_NT-10.0)
  FORMAT=win64
  WINSTUFF=-target i686-pc-mingw64
endif
endif
endif

SRC_DIR :=./tests/input
SRC_FILES := $(wildcard $(SRC_DIR)/*.adder)
OUT_DIR :=./tests/output
OUT_FILES := $(wildcard $(SRC_DIR)/*.result)
ASM_FILES := $(patsubst $(SRC_DIR)/%.$(EXT),$(OUT_DIR)/%.s,$(SRC_FILES))

test-asm: $(ASM_FILES)
	echo $(ASM_FILES)

.PHONY: clean

test: clean
	$(TEST)


bin:
	$(BUILD)

clean:
	rm -rf tests/output/*.o tests/output/*.s tests/output/*.dSYM tests/output/*.run tests/output/*.log tests/output/*.result tests/output/*.$(COMPILER) tests/output/*.result

distclean: clean
	$(CLEAN)

ghci: init.txt
	$(REPL) $(BUILD_OPTS)

init.txt:
	$(UPDATE) > init.txt

turnin: 
	git commit -a -m "turnin"
	git push origin main

upstream:
	git remote add upstream https://github.com/$(ORG)/$(ASGN)-$(COMPILER).git

update:
	git pull upstream main --allow-unrelated-histories

tests/output/%.result: tests/output/%.run
	$< > $@

tests/output/%.run: tests/output/%.o c-bits/main.c
	clang $(WINSTUFF) -g -m64 -o $@ c-bits/main.c $<

tests/output/%.o: tests/output/%.s
	nasm -f $(FORMAT) -o $@ $<

tests/output/%.s: tests/input/%.$(EXT)
	$(COMPILEREXEC) $< > $@
