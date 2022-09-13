#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>

int main(int argc, char **argv)
{

    int c;

    while ((c = getchar()) != EOF)
    {
        if (isdigit(c))
        {
            char count = c - '0';
            if ((c = getchar()) == EOF)
            {
                return 0;
            }
            for (char i = 0; i < count; i++)
            {
                putchar(c);
            }
        }
        // alyaws
        putchar(c);
    }

    return 0;
}
