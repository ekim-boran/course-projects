#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

// MWC64X RNG from David B. Thomas.
// See http://cas.ee.ic.ac.uk/people/dt10/research/rngs-gpu-mwc64x.html
uint32_t MWC64X(uint64_t *state)
{
    uint32_t c=(*state)>>32, x=(*state)&0xFFFFFFFF;
    *state = x*((uint64_t)4294883355U) + c;
    return x^c;
}

#define BASE (10)            // for strtoll
#define INITIAL_STATE (137)  // for PRNG
#define NUM_COMMANDS (3)
#define INSERT_COMMAND (0)
#define COMMANDS ("/+-")
#define NEWLINE_CHANCE (3)

// generate random edit string
// followed by a print command
//
// n = number of cursors
// length = number of edits
//
// inserts will only insert letters and newlines
int
main(int argc, char **argv)
{

    if(argc != 3) {
        fprintf(stderr, "Usage: %s number-of-cursors number-of-edit-instructions\n", argv[0]);
        return 1;
    }

    int n = atoi(argv[1]);
    size_t length = strtoll(argv[2], 0, BASE);

    uint64_t state = INITIAL_STATE; 

    for(size_t i = 0; i < length; i++) {
        // using % may create a small bias,
        // but we are not doing serious simulation here
        uint32_t cursor = MWC64X(&state) % n;

        if(cursor != 0) {
            printf("%u", cursor);
        }

        int command = MWC64X(&state) % NUM_COMMANDS;

        putchar(COMMANDS[command]);

        if(command == INSERT_COMMAND) {
            int letter = 'A' + MWC64X(&state) % ('Z' - 'A' + 1 + NEWLINE_CHANCE);
            if(letter > 'Z') {
                putchar('\n');
            } else {
                putchar(letter);
            }
        }

        putchar('\n');  // will be ignored by editor
    }

    // add in a victory speech and print final string
    puts("/ />/>/>/ /A/ /w/i/n/n/e/r/ /i/s/ /y/o/u/!/ /</</</ .");

    return 0;
}
