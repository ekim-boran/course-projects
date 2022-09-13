#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include "wordArray.h"

// increment key and print result if no overflow
// else print "-OVERFLOW-"
int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: %s key-to-increment\n", argv[0]);
        return 1;
    }

    if (wordArrayIncKey(argv[1]) == 0)
    {
        puts(argv[1]);
    }
    else
    {
        puts("-OVERFLOW-");
    }

    return 0;
}
