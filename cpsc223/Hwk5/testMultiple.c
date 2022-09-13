#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "wordArray.h"

// create a bunch of WordArrays and test them all
int
main(int argc, char **argv)
{
    if(argc != 3) {
        fprintf(stderr, "Usage: %s key-length number-of-arrays\n", argv[0]);
        return 1;
    }

    // make a bunch of wordArrays
    // and test them all
    int n = atoi(argv[1]);
    int count = atoi(argv[2]);

    WordArray *ww = malloc(sizeof(WordArray) * count);
    int fill = 0;

    // will with increasing numbers across all
    for(int i = 0; i < count; i++) {
        ww[i] = wordArrayCreate(n);

        char *key = wordArrayMinKey(ww[i]);


        do {
            *wordArrayRef(ww[i], key) = fill++;
        } while(wordArrayIncKey(key) == 0);

        free(key);
    }

    // reset fill and test
    fill = 0;
    for(int i = 0; i < count; i++) {

        char *key = wordArrayMinKey(ww[i]);

        do {
            assert(*wordArrayRef(ww[i], key) == fill++);
        } while(wordArrayIncKey(key) == 0);

        free(key);
    }

    // cleanup
    for(int i = 0; i < count; i++) {
        wordArrayDestroy(ww[i]);
    }

    free(ww);

    return 0;
}
