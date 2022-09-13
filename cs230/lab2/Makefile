#
# Makefile that builds btest and other helper programs for the CS:APP data lab
# 
CC = gcc
CFLAGS = -O -m32 -Wall -Wextra -Wno-unused-parameter -Wno-cast-function-type -Wno-unused-variable -Wno-implicit-function-declaration -Werror -pedantic -fno-builtin-printf -fsanitize=address
LIBS = -lm

# C formatting related constants
TARGET = .*\.\(cpp\|hpp\|c\|h\)
STYLE="{BasedOnStyle: llvm, AllowShortFunctionsOnASingleLine: None, SortIncludes: false}"
CFORMAT = clang-format-10

all: fshow ishow format grade

btest: btest.c bits.c decl.c tests.c btest.h bits.h
	$(CC) $(CFLAGS) $(LIBS) -o btest bits.c btest.c decl.c tests.c

fshow: fshow.c
	$(CC) $(CFLAGS) -o fshow fshow.c

ishow: ishow.c
	$(CC) $(CFLAGS) -o ishow ishow.c

grade: btest dlc format-check
	./driver.pl

# Forces a recompile. Used by the driver program. 
btestexplicit:
	$(CC) $(CFLAGS) $(LIBS) -o btest bits.c btest.c decl.c tests.c

format:
	#######################################################
	# Process formatting according to the clang format file
	#######################################################
	@find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) -i && echo "Completed formatting"

format-check:
	#################################
	# Check the code is in the format
	#################################
	@find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) --dry-run --Werror -i && echo "Everything is in the format"

clean:
	rm -f *.o btest fshow ishow *~
