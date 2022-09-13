#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>

// Takes a salt string from argv[1], and an unencrypted password
// file with lines of the form
//
//     username:password
//
// from stdin, and sends to stdout out and encrypted password
// file with lines of the form
//
//     username:encrypted-password
//
// Each encrypted password is obtained by running a 64-bit FNV1-a
// hash on the salt followed by the unencrypted password, and
// printing the result in hexadecimal.


#define FNV_PRIME_64 ((1ULL<<40)+(1<<8)+0xb3)
#define FNV_OFFSET_BASIS_64 (14695981039346656037ULL)

// modified version of FNV1a that updates hash
// based on string s
static void
FNV1a(uint64_t *hash, const char *s)
{
    while(*s) {
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
getLine(void)
{
    int c;
    
    // invariant:
    // size > top
    // size = size of buffer
    size_t size = GETLINE_INITIAL_SIZE;
    size_t top = 0;
    char *buffer = calloc(size, sizeof(char));

    assert(buffer);

    for(;;) {
        switch((c = getchar())) {

            case EOF:
                if(top == 0) {
                    // got nothing
                    free(buffer);
                    return 0;
                }
                // else fall through

            case '\n':
                buffer[top] = '\0';
                return buffer;

            default:
                buffer[top++] = c;
                if(top >= size) {
                    size *= GETLINE_MULTIPLIER;
                    buffer = realloc(buffer, size * sizeof(char));

                    assert(buffer);
                }
        }
    }
}

#define PASSWORD_DELIMITER (':')

int
main(int argc, char **argv)
{

    if(argc != 2) {
        fprintf(stderr, "Usage: %s salt < password-file > encrypted-password-file\n", argv[0]);
        return 1;
    }

    // precompute salt
    uint64_t saltHash = precomputeSaltHash(argv[1]);

    int c;

    // main loop: copy username, then encrypt password
    while((c = getchar()) != EOF) {
        // copy to stdout
        putchar(c);

        if(c == PASSWORD_DELIMITER) {
            // process password
            char *password = getLine();

            // behavior on EOL is undefined,
            // we'll just quit
            assert(password);

            uint64_t hash = passwordHash(saltHash, password);

            printf("%" PRIx64 "\n", hash);

            free(password);
        } 
    }

    return 0;
}
