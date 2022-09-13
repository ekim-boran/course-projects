#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "wordArray.h"

int
main(int argc, char **argv)
{

    if(argc != 3) {
        fprintf(stderr, "Usage: %s width key\n", argv[0]);
        return 1;
    }

    WordArray w = wordArrayCreate(atoi(argv[1]));

    int *p = wordArrayRef(w, argv[2]);

    if(p) {
        printf("found %d\n", *p);
    } else {
        puts("bad key");
    }

    return 0;
}
