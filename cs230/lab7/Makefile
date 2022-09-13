# Makefile for Proxy Lab

CC = gcc
CFLAGS = -g -Wall -Wextra -Wno-unused-parameter -Werror -pedantic #-fsanitize=address
LDFLAGS = -lpthread

# C formatting related constants
TARGET = .*\.\(cpp\|hpp\|c\|h\)
STYLE="{BasedOnStyle: llvm, AllowShortFunctionsOnASingleLine: None, SortIncludes: false}"
CFORMAT = clang-format-10

all: format grade

grade: proxy format-check
	./driver.sh

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

proxy.o: proxy.c csapp.h
	$(CC) $(CFLAGS) -c proxy.c

proxy: proxy.o csapp.o
	$(CC) $(CFLAGS) proxy.o csapp.o -o proxy $(LDFLAGS)

format:
	#######################################################
	# Process formatting according to the clang format file
	#######################################################
	# @find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) -i && echo "Completed formatting"

format-check:
	#################################
	# Check the code is in the format
	#################################
	# @find . -regex '$(TARGET)' | xargs $(CFORMAT) --style=$(STYLE) --dry-run --Werror -i && echo "Everything is in the format"

clean:
	rm -f *~ *.o proxy core *.tar *.zip *.gzip *.bzip *.gz
