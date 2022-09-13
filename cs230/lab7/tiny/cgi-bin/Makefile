CC = gcc
CFLAGS = -O2 -I .. -Wall -Wextra -Wno-format-overflow -Wno-restrict -Werror -pedantic -fsanitize=address

all: adder

adder: adder.c
	$(CC) $(CFLAGS) -o adder adder.c

clean:
	rm -f adder *~
