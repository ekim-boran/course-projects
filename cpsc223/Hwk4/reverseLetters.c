#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <ctype.h>

char *
getInput(size_t *n)
{
    size_t size = 32;
    char *buf = malloc(size);
    int c;

    *n = 0;

    while ((c = getchar()) != EOF)
    {
        if (*n >= size)
        {
            size *= 2;
            buf = realloc(buf, size);
        }
        buf[*n] = c;
        (*n)++;
    }
    return buf;
}

char to_upper(char str)
{
    if (isupper(str))
    {
        return str;
    }
    else
    {
        return str + ('A' - 'a');
    }
}

char to_lower(char str)
{
    if (islower(str))
    {
        return str;
    }
    else
    {
        return str + ('a' - 'A');
    }
}

void reverse(char *start, char *end)
{
    while (1)
    {
        while (start < end && !isalpha(*start))
        {
            start++;
        }
        while (start < end && !isalpha(*end))
        {
            end--;
        }
        if (start >= end)
        {
            return;
        }
        char start_temp = *start;
        char end_temp = *end;

        if (isupper(start_temp))
        {
            *start = to_upper(end_temp);
        }
        else
        {
            *start = to_lower(end_temp);
        }
        if (isupper(end_temp))
        {
            *end = to_upper(start_temp);
        }
        else
        {
            *end = to_lower(start_temp);
        }

        start++;
        end--;
    }
}

int main(int argc, char **argv)
{

    if (argc != 1)
    {
        fprintf(stderr, "Usage: %s\n", argv[0]);
        return 1;
    }

    size_t n;
    char *s = getInput(&n);
    if (n != 0)
    {
        reverse(s, s + n - 1);
        for (int i = 0; i < n; i++)
        {
            putchar(s[i]);
        }
    }

    free(s);

    return 0;
}