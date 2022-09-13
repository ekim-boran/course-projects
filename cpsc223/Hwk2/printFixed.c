#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

// print without buffering to intermediate

unsigned int size(long long number)
{
    if (number < 10 && number > -10)
    {
        return 1;
    }
    else
    {
        return 1 + size(number / 10);
    }
}

long long mask(unsigned int sz)
{
    long long number = 1;
    for (unsigned int a = 0; a < sz; a++)
    {
        number *= 10;
    }
    return number;
}

void print_num(long long num, unsigned int l)
{
    if (l == 3)
    {
        putchar(((char)(num / 100)) + '0');
        num = num % 100;
        l--;
    }
    if (l == 2)
    {
        putchar(((char)(num / 10)) + '0');
        num = num % 10;
        l--;
    }
    if (l == 1)
    {
        putchar(((char)(num)) + '0');
    }
}

long long print_number(long long number, unsigned int start, unsigned int end, char seperator)
{
    if (start == end)
    {
        return number;
    }
    int print_size = (start - end) % 3;
    if (print_size == 0)
    {
        print_size = 3;
    }
    long long ms = mask(start - print_size);
    long long n = number / ms;
    print_num(n < 0 ? -n : n, print_size);
    number = number - (ms * n);
    start = start - print_size;

    while (start > end)
    {
        if (seperator != 0)
        {
            putchar(seperator);
        }
        start = start - 3;
        long long ms = mask(start);
        long long n = number / ms;
        print_num(n < 0 ? -n : n, 3);
        number = number - (ms * n);
    }

    return number;
}

void printFixed(long long number, char separator, char decimalPoint, unsigned int precision)
{
    if (number < 0)
    {
        putchar('-');
    }
    unsigned int sz = size(number);

    if (sz <= precision)
    {
        putchar('0');
        putchar(decimalPoint);
        for (int i = 0; i < precision - sz; i++)
        {
            putchar('0');
        }
        print_number(number, sz, 0, 0);
    }
    else
    {
        long long n = print_number(number, sz, precision, separator);
        putchar(decimalPoint);
        print_number(n, precision, 0, 0);
    }
}
