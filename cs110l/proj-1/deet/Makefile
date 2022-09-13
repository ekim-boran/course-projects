SRCS = $(wildcard samples/*.c)
PROGS = $(patsubst %.c,%,$(SRCS))

all: $(PROGS)

%: %.c
	$(CC) $(CFLAGS) -O0 -g -no-pie -fno-omit-frame-pointer -o $@ $<
	objdump -S $@ >  $<.asm
clean:
	rm -f $(PROGS)
