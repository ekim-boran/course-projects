#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>

#include "buffer.h"

#define RADIX (10) // base for cursor numbers

// Minimalist text editor with optional multiple buffers.
//
// Each command is preceded by a number in decimal specifying a cursor.
// If number is omitted, defaults to 0.
//
// commands are
//
// <n>/<char> insert char before cursor n; if <char> is EOF, exit
// <n>+       move cursor n forward
// <n>-       move cursor n back
// .          print buffer contents
// #          comment: consume all characters to EOF or '\n'
//
// Any character that is not a digit or part of a command is ignored.
//
// Commands to cursor i go to buffer i % m, where m is a command-line argument.
int main(int argc, char **argv)
{
    int m; // number of buffers

    switch (argc)
    {

    case 2:

        m = 1;
        break;

    case 3:

        m = atoi(argv[2]);
        break;

    default:

        fprintf(stderr, "Usage: %s number-of-cursors [number-of-buffers]\n", argv[0]);
        return 1;
    }

    Buffer b[m];
    for (int i = 0; i < m; i++)
    {
        b[i] = bufferCreate(atoi(argv[1]));
    }
    int c;
    size_t cursor = 0;
    char *contents; // for printing buffer contents

    while ((c = getchar()) != EOF)
    {
        if (isdigit(c))
        {
            cursor = cursor * RADIX + (c - '0');
        }
        else
        {
            switch (c)
            {

            case '/':
                if ((c = getchar()) == EOF)
                {
                    goto done;
                }
                bufferInsert(b[cursor % m], cursor, c);

                cursor = 0;
                break;

            case '+':

                bufferCursorForward(b[cursor % m], cursor);
                cursor = 0;
                break;

            case '-':

                bufferCursorBack(b[cursor % m], cursor);
                cursor = 0;
                break;

            case '.':

                contents = bufferContents(b[cursor % m]);
                fputs(contents, stdout);
                free(contents);
                break;

            default:

                break;
            }
        }
    }

done:

    for (int i = 0; i < m; i++)
    {
        printf("\n[%zu cursors and %zu characters]\n", bufferCursors(b[i]), bufferSize(b[i]));
        bufferDestroy(b[i]);
    }

    return 0;
}
