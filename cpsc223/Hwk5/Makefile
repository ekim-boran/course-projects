CC=c99
CFLAGS=-g3 -Wall

PROGRAMS=testCreateDestroy testMinKey testIncKey testArrayRef testPrint testFill testMultiple ngrams

all: $(PROGRAMS)

test: all
	# run some basic tests
	./testCreateDestroy 1
	./testMinKey 2
	./testIncKey qrz
	./testArrayRef 2 ab
	./testArrayRef 2 00
	./testArrayRef 2 AZ
	./testArrayRef 2 a
	./testArrayRef 2 abc
	./testPrint 1
	./testFill 1
	./testMultiple 3 5
	./ngrams 1 < ngrams.c

# dependencies for programs
testCreateDestroy: testCreateDestroy.o wordArray.o
testMinKey: testMinKey.o wordArray.o
testIncKey: testIncKey.o wordArray.o
testArrayRef: testArrayRef.o wordArray.o
testPrint: testPrint.o wordArray.o
testFill: testFill.o wordArray.o
testMultiple: testMultiple.o wordArray.o
ngrams: ngrams.o wordArray.o

# dependencies for .o files
wordArray.o: wordArray.c wordArray.h
testCreateDestroy.o: testCreateDestroy.c wordArray.h
testMinKey.o: testMinKey.c wordArray.h
testIncKey.o: testIncKey.c wordArray.h
testArrayRef.o: testArrayRef.c wordArray.h
testPrint.o: testPrint.c wordArray.h
testFill.o: testFill.c wordArray.h
testMultiple.o: testMultiple.c wordArray.h
ngrams.o: ngrams.c wordArray.h

clean:
	$(RM) *.o $(PROGRAMS)
