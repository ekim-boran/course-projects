.DEFAULT_GOAL := all
CC = gcc
CFLAGS = -Wall -Wextra -Werror -pedantic # -fsanitize=address
CXX = g++
CXXFLAGS = -Wall -Wextra -Werror -pedantic# -fsanitize=address
LDFLAGS = -lgtest -pthread

# C formatting related constants
TARGET=.*\.\(cpp\|hpp\|c\|h\)
STYLE="{BasedOnStyle: llvm, AllowShortFunctionsOnASingleLine: None, SortIncludes: false}"
CFORMAT=clang-format-10

all: list.o test format grade

test: list.o test.cpp
	$(CXX) $(CXXFLAGS) -o test list.o test.cpp $(LDFLAGS)

test_run: test
	@./grade.sh

grade: format-check test_run

list.o: list.c list.h
	$(CC) $(CFLAGS) -c list.c

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
	###########################################################
	# Clean the first-level of the linkedlistlab directory tree
	###########################################################
	rm -f *.o test grade
