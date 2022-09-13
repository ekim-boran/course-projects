#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>

#define FNV_PRIME_64 ((1ULL << 40) + (1 << 8) + 0xb3)
#define FNV_OFFSET_BASIS_64 (14695981039346656037ULL)

// modified version of FNV1a that updates hash
// based on string s
static void
FNV1a(uint64_t *hash, const char *s)
{
    while (*s)
    {
        *hash ^= *s++;
        *hash *= FNV_PRIME_64;
    }
}

// precompute result of applying salt
static uint64_t
precomputeSaltHash(const char *salt)
{
    uint64_t hash = FNV_OFFSET_BASIS_64;

    FNV1a(&hash, salt);

    return hash;
}

// hash string with precomputed salt
static uint64_t
passwordHash(uint64_t saltHash, const char *password)
{
    FNV1a(&saltHash, password);

    return saltHash;
}

#define GETLINE_INITIAL_SIZE (16)
#define GETLINE_MULTIPLIER (2)

// fetch characters from stdin to first newline or EOL
// returns malloc'd buffer that should be freed by caller
// if EOL is first character, returns 0
static char *
getWord(FILE *f)
{
    int c;
    // invariant:
    // size > top
    // size = size of buffer
    size_t size = GETLINE_INITIAL_SIZE;
    size_t top = 0;
    char *buffer = calloc(size, sizeof(char));

    assert(buffer);

    for (;;)
    {
        switch ((c = getc(f)))
        {

        case EOF:
            if (top == 0)
            {
                free(buffer);
                return 0;
            }
            // else fall through

        case '\n':
            buffer[top] = '\0';
            return buffer;

        default:
            buffer[top++] = c;
            if (top >= size)
            {
                size *= GETLINE_MULTIPLIER;
                buffer = realloc(buffer, size * sizeof(char));

                assert(buffer);
            }
        }
    }
}

typedef struct Node
{
    uint64_t hash;
    char *password;
    struct Node *next;
} Node;

typedef struct Dict
{
    size_t capacity;
    size_t len;
    Node **buffer;
} Dict;

Dict *create_dict(size_t len)
{
    Dict *d = calloc(1, sizeof(Dict));
    d->capacity = len;
    d->len = 0;
    d->buffer = calloc(len, sizeof(Node *));
    return d;
}

void destroy_helper(Node **buffer, size_t cap)
{
    for (int i = 0; i < cap; i++)
    {
        Node *head = buffer[i];
        while (head != 0)
        {
            Node *next = head->next;
            free(head);
            head = next;
        }
    }
    free(buffer);
}

void destroy_dict(Dict *d)
{
    for (int i = 0; i < d->capacity; i++)
    {
        Node *head = d->buffer[i];
        while (head != 0)
        {
            Node *next = head->next;
            free(head->password);
            free(head);

            head = next;
        }
    }
    free(d->buffer);

    free(d);
}

void _insert(Dict *d, uint64_t hash, char *password)
{
    Node *head = d->buffer[hash % d->capacity];
    Node *newhead = calloc(1, sizeof(Node));
    newhead->hash = hash;
    newhead->password = password;
    newhead->next = head;
    d->len = d->len + 1;
    d->buffer[hash % d->capacity] = newhead;
}

void insert(Dict *d, uint64_t hash, char *password)
{
    if (d->len >= d->capacity * 2)
    {
        Node **oldbuffer = d->buffer;
        size_t oldcapacity = d->capacity;
        d->capacity = oldcapacity * 2;
        d->buffer = calloc(d->capacity, sizeof(Node *));
        d->len = 0;

        for (int i = 0; i < oldcapacity; i++)
        {

            Node *head = oldbuffer[i];
            while (head != 0)
            {
                Node *next = head->next;
                _insert(d, head->hash, head->password);
                head = next;
            }
        }

        destroy_helper(oldbuffer, oldcapacity);
    }
    _insert(d, hash, password);
}

char *find(Dict *d, uint64_t hash)
{
    Node *head = d->buffer[hash % d->capacity];
    while (head != 0)
    {
        if (head->hash == hash)
        {
            return head->password;
        }
        head = head->next;
    }
    return 0;
}

Dict *create_dictionary(FILE *f, uint64_t saltHash)
{
    Dict *dict = create_dict(4);
    char *word;
    while ((word = getWord(f)) != 0)
    {
        uint64_t hash = passwordHash(saltHash, word);
        insert(dict, hash, word);
    }
    return dict;
}

#define PASSWORD_DELIMITER (':')

int main(int argc, char **argv)
{

    // precompute salt
    uint64_t saltHash = precomputeSaltHash(argv[1]);
    FILE *f;
    if ((f = fopen(argv[2], "r")) == 0)
    {
        fprintf(stderr, "Cannot open: %s  ", argv[2]);
        return 1;
    }

    Dict *d = create_dictionary(f, saltHash);

    int c;

    // main loop: copy username, then encrypt password
    while ((c = getchar()) != EOF)
    {
        // copy to stdout
        putchar(c);

        if (c == PASSWORD_DELIMITER)
        {
            // process password
            char *hash = getWord(stdin);
            uint64_t hash_num = strtoul(hash, 0, 16);
            char *password = find(d, hash_num);
            if (password == 0)
            {
                printf("%s\n", hash);
            }
            else
            {
                printf("%s\n", password);
            }
            free(hash);
        }
    }
    destroy_dict(d);

    return 0;
}
