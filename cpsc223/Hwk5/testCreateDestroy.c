#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "wordArray.h"

// make a WordArray with given length,
// print its length,
// and free it
int
main(int argc, char **argv)
{
    if(argc != 2) {
        fprintf(stderr, "Usage: %s key-length\n", argv[0]);
        return 1;
    }
    
    WordArray w = wordArrayCreate(atoi(argv[1]));

    assert(w);

    printf("%d\n", wordArrayKeyLength(w));

    wordArrayDestroy(w);

    return 0;
}
