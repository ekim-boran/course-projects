#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "printFixed.h"
int main(int argc, char **argv)
{
    if (argc != 5)
    {
        fprintf(stderr, "argument error");
        return 1;
    }

    long long number = strtoll(argv[1], 0, 10);
    char separator = argv[2][0];
    char decimalPoint = argv[3][0];
    int precision = atoi(argv[4]);

    printFixed(number, separator, decimalPoint, precision);
    putchar('\n');

    return 0;
}
