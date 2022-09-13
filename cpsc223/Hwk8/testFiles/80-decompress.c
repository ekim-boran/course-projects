#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

// Decompress compressed file on stdin,
// sending results to stdout.
//
// Compression format is described in
// compressionFormat.h.

#include "compressionFormat.h"

int
main(int argc, char **argv)
{

    if(argc != 1) {
        fprintf(stderr, "Usage: %s < compressed-data > uncompressed-data\n", argv[0]);
        return 1;
    }

    // read the dictionary
    char dictionary[COMPRESSED_CHARS][COMPRESSED_EXPANSION];
    size_t dictionaryItemsRead;   // how many entries we got

    if((dictionaryItemsRead = fread(dictionary, COMPRESSED_EXPANSION, COMPRESSED_CHARS, stdin)) != COMPRESSED_CHARS) {
        // didn't get enough
        fprintf(stderr, "compression dictionary: expected %d entries, got %zu\n", COMPRESSED_CHARS, dictionaryItemsRead);
        return 1;
    }

    // process the rest
    int c;

    while((c = getchar()) != EOF) {
        if(c == COMPRESSED_CHAR_ESCAPE) {
            // output next character
            if((c = getchar()) != EOF) {
                putchar(c);
            } else {
                break;
            }
        } else if(c >= COMPRESSED_CHAR_MIN && c <= COMPRESSED_CHAR_MAX) {
            // output expansion
            for(int i = 0; i < COMPRESSED_EXPANSION; i++) {
                putchar(dictionary[c - COMPRESSED_CHAR_MIN][i]);
            }
        } else {
            // output the character
            putchar(c);
        }
    }

    return 0;
}
