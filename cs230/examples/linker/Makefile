CC = gcc
CFLAGS = -Wall -Wextra -Wpedantic -Wconversion -Og

SRCS = $(wildcard *.c)
OBJS = $(patsubst %.c,%.o,$(SRCS))

all: main

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

main: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o main

.PHONY: clean
clean:
	rm -rf $(OBJS) main
