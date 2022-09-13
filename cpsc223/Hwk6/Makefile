CC=c99
CFLAGS=-g3 -Wall

PROGRAMS=editor randomizer

all: $(PROGRAMS)

test: all
	echo '/t/h/e/r/e------/H/i/ 17+17/!35/X.' | ./editor 23
	./randomizer 100 200 | ./editor 87
	./randomizer 10 10000000 | time ./editor 9 > /dev/null
	./randomizer 10 1000 | valgrind -q --tool=memcheck --leak-check=yes ./editor 7

# dependencies for programs
editor: editor.o buffer.o
randomizer.o: randomizer.c

# dependencies for .o files
editor.o: editor.c buffer.h
buffer.o: buffer.c buffer.h
randomizer.o: randomizer.c

clean:
	$(RM) *.o $(PROGRAMS)
