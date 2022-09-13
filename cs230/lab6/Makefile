#
# Students' Makefile for the Malloc Lab
#
TEAM = bovik
VERSION = 1
HANDINDIR = /afs/cs.cmu.edu/academic/class/15213-f01/malloclab/handin

CC = gcc 
CFLAGS = -O3 -m32 -Wall -Wextra -Wno-unused-parameter -Wno-unused-result -Wno-format-overflow -Werror -pedantic  -fsanitize=address

OBJS = mdriver.o mm.o memlib.o fsecs.o fcyc.o clock.o ftimer.o

# C formatting related constants
TARGET = .*\.\(cpp\|hpp\|c\|h\)
STYLE="{BasedOnStyle: llvm, AllowShortFunctionsOnASingleLine: None, SortIncludes: false}"
CFORMAT = clang-format-10

all: mdriver format grade

grade: format-check mdriver
	./mdriver

mdriver: $(OBJS)
	$(CC) $(CFLAGS) -o mdriver $(OBJS)

mdriver.o: mdriver.c fsecs.h fcyc.h clock.h memlib.h config.h mm.h
memlib.o: memlib.c memlib.h
mm.o: mm.c mm.h memlib.h
fsecs.o: fsecs.c fsecs.h config.h
fcyc.o: fcyc.c fcyc.h
ftimer.o: ftimer.c ftimer.h config.h
clock.o: clock.c clock.h

handin:
	cp mm.c $(HANDINDIR)/$(TEAM)-$(VERSION)-mm.c

format:
	#######################################################
	# Process formatting according to the clang format file
	#######################################################
#	@find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) -i && echo "Completed formatting"

format-check:
	#################################
	# Check the code is in the format
	#################################
#	@find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) --dry-run --Werror -i && echo "Everything is in the format"

clean:
	rm -f *~ *.o mdriver
