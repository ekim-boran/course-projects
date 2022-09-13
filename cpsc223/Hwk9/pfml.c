#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

static unsigned char *
readAll(FILE *f, size_t *n)
{
    size_t size = 16;
    unsigned char *buffer = malloc(size);
    *n = 0;

    int c;
    while ((c = getc(f)) != EOF)
    {

        if (*n >= size)
        {
            size *= 2;
            buffer = realloc(buffer, size);
        }
        buffer[(*n)++] = c;
    }

    return buffer;
}

typedef struct
{
    int cap;
    int len;
    char *ptr;
} Vec;

Vec *create_vec()
{
    Vec *v = calloc(1, sizeof(Vec));
    v->cap = 4;
    v->len = 0;

    v->ptr = calloc(4, 1);
    return v;
}
void destroy_vec(Vec *v)
{
    free(v->ptr);
    free(v);
}

void append(Vec *v, char c)
{
    if (v->len == v->cap)
    {
        v->ptr = realloc(v->ptr, v->cap * 2);
        v->cap = v->cap * 2;
    }
    v->ptr[v->len] = c;
    v->len++;
}

void reverse(Vec *v)
{
    if (v->len <= 1)
    {
        return;
    }
    int i = 0;
    int j = v->len - 1;
    while (i < j)
    {
        char c = v->ptr[i];
        v->ptr[i] = v->ptr[j];
        v->ptr[j] = c;
        i++;
        j--;
    }
}

Vec *process(unsigned char *str, size_t n, int first, size_t *processed)
{
    Vec *v = create_vec();
    while (1)
    {
        while (*processed < n && str[*processed] != '{' && str[*processed] != '}')
        {
            append(v, str[*processed]);
            (*processed)++;
        }
        if (*processed == n)
        {
            break;
        }
        else if (str[*processed] == '{')
        {
            (*processed)++;
            Vec *r = process(str, n, 0, processed);
            for (int i = 0; i < r->len; i++)
            {
                append(v, r->ptr[i]);
            }
            destroy_vec(r);
        }
        else if (str[*processed] == '}')
        {
            (*processed)++;
            break;
        }
    }
    if (!first)
    {
        reverse(v);
    }
    return v;
}

int main()
{
    size_t n;
    unsigned char *str = readAll(stdin, &n);
    size_t processed = 0;
    Vec *v = process(str, n, 1, &processed);
    for (int i = 0; i < v->len; i++)
    {
        putchar(v->ptr[i]);
    }
    destroy_vec(v);
    free(str);
}