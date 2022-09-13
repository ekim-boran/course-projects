#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>

#include "wordArray.h"

int
main(int argc, char **argv)
{

    if(argc != 2) {
        fprintf(stderr, "Usage: %s n\n", argv[0]);
        return 1;
    }

    // compute counts of all the n-grams in stdin
    int n = atoi(argv[1]);
    char buffer[n+1];

    for(int i = 0; i < n+1; i++) { 
        buffer[i] = '\0';
    }

    WordArray w = wordArrayCreate(n);

    int c;

    while((c = getchar()) != EOF) {
        if(isalpha(c)) {
            if(isupper(c)) {
                c = tolower(c);
            }

            // shift into buffer
            for(int i = 0; i < n-1; i++) {
                buffer[i] = buffer[i+1];
            }
            buffer[n-1] = c;

            if(buffer[0]) {
                (*wordArrayRef(w, buffer))++;
            }
        }
    }

    wordArrayPrint(w);

    wordArrayDestroy(w);

    return 0;
}
