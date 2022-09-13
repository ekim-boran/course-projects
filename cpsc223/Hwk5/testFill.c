#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "wordArray.h"

// fill a WordArray with a test pattern then print it
int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s key-length\n", argv[0]);
        return 1;
    }

    WordArray w = wordArrayCreate(atoi(argv[1]));

    assert(w);

    int fill = 17;

    char *key = wordArrayMinKey(w);

    do
    {
        *wordArrayRef(w, key) = (fill += 3);
    } while (wordArrayIncKey(key) == 0);

    wordArrayPrint(w);

    free(key);
    wordArrayDestroy(w);

    return 0;
}
