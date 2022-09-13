#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>

void print_buffer(unsigned int current_char, unsigned int current_count)
{
    if (current_char == EOF || current_count == 0)
    {
        return;
    }
    if (isdigit(current_char) || current_count > 2)
    {
        putchar(current_count - 1 + '0');
    }
    else if (current_count == 2)
    {
        putchar(current_char);
    }
    putchar(current_char);
}

int main(int argc, char **argv)
{
    unsigned int c;
    unsigned int current_char = EOF;
    unsigned int current_count = 0;
    while ((c = getchar()) != EOF)
    {
        if (c == current_char && current_count < 10)
        {
            current_count++;
        }
        else
        {
            print_buffer(current_char, current_count);
            current_char = c;
            current_count = 1;
        }
    }

    print_buffer(current_char, current_count);
    return 0;
}
