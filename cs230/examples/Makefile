CC = gcc
CFLAGS = -Wall -Wextra -Wpedantic -Wconversion -Og

LIB_SRCS = s04-sum.c s04-swap.c s04-leaq.c s05-control.c
LIB_OBJS = $(patsubst %.c,%.o,$(LIB_SRCS))
LIB_ASMS = $(patsubst %.c,%.s,$(LIB_SRCS))
LIB_DISASMS = $(patsubst %.c,%.dis.s,$(LIB_SRCS))

PROG_SRCS = $(filter-out $(LIB_SRCS), $(wildcard *.c))
PROGS = $(patsubst %.c,%,$(PROG_SRCS))

all: $(PROGS) $(LIB_OBJS) $(LIB_ASMS) $(LIB_DISASMS)

%: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.s: %.c
	$(CC) $(CFLAGS) -S -fverbose-asm -o $@ $<

%.dis.s: %.o
	objdump -d $< > $@

.PHONY: clean
clean:
	rm -rf $(PROGS) $(LIB_OBJS) $(LIB_ASMS) $(LIB_DISASMS)
