CC = gcc
CFLAGS = -O2 -I . -Wall -Wextra -Wno-format-overflow -Werror -pedantic -fsanitize=address

# This flag includes the Pthreads library on a Linux box.
# Others systems will probably require something different.
LIB = -lpthread

all: tiny cgi

tiny: tiny.c csapp.o
	$(CC) $(CFLAGS) -o tiny tiny.c csapp.o $(LIB)

csapp.o: csapp.c
	$(CC) $(CFLAGS) -c csapp.c

cgi:
	make -C cgi-bin

clean:
	rm -f *.o tiny *~
	make -C cgi-bin clean

