#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "wordArray.h"

// print an unmodified WordArray
int
main(int argc, char **argv)
{
    if(argc != 2) {
        fprintf(stderr, "Usage: %s key-length\n", argv[0]);
        return 1;
    }

    WordArray w = wordArrayCreate(atoi(argv[1]));

    assert(w);

    wordArrayPrint(w);

    wordArrayDestroy(w);

    return 0;
}
