#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <inttypes.h>

#include "orderBook.h"

// MWC64X RNG from David B. Thomas.
// See http://cas.ee.ic.ac.uk/people/dt10/research/rngs-gpu-mwc64x.html
static uint32_t 
MWC64X(uint64_t *state)
{
    uint32_t c=(*state)>>32, x=(*state)&0xFFFFFFFF;
    *state = x*((uint64_t)4294883355U) + c;
    return x^c;
}

// Process orders from stdin for multiple
// orderBooks.
//
// Each order has book number and price
// formatted as %zu %d.
//
// Returns 0 on EOF or parse error.
static int
testOrdersInput(size_t numBooks)
{
    OrderBook *b = calloc(numBooks, sizeof(OrderBook));
    for(size_t i = 0; i < numBooks; i++) {
        b[i] = orderBookCreate();
    }

    size_t whichBook;
    int order;

    while(scanf("%zu%d", &whichBook, &order) == 2) {
        // ignore orders to books that don't exist
        if(whichBook < numBooks) {
            printf("%zu %d %d\n", 
                    whichBook, 
                    order, 
                    orderBookInsert(b[whichBook], order));
        }
    }

    for(size_t i = 0; i < numBooks; i++) {
        orderBookDestroy(b[i]);
    }

    free(b);

    return 0;
}

#define FNV_PRIME_64 ((1ULL<<40)+(1<<8)+0xb3)
#define FNV_OFFSET_BASIS_64 (14695981039346656037ULL)

static void
FNV1aUpdate(uint64_t *h, size_t n, unsigned char *bytes)
{
    for(size_t i = 0; i < n; i++) {
        *h ^= bytes[i];
        *h *= FNV_PRIME_64;
    }
}

// Performance test.
//
// Using given initial see for MWC64X RNG,
// generate numOrders random orders in range
// [-maxOrder..maxOrder].
//
// At end, prints total profit and a hash of all matches
// then returns 0.
static int
testOrdersRandom(uint64_t seed, size_t orders, int maxOrder)
{
    uint64_t h = FNV_OFFSET_BASIS_64;
    uint32_t range = 2*maxOrder + 1;
    OrderBook b = orderBookCreate();
    unsigned long long totalProfit = 0;

    for(size_t i = 0; i < orders; i++) {
        // generate an order
        int price = MWC64X(&seed) % range - maxOrder - 1;
        int match = orderBookInsert(b, price);

        if(match != 0) {
            totalProfit += (price + match);
        } 
            

        FNV1aUpdate(&h, sizeof(match), (unsigned char *) &match);
    }

    printf("Total profit: %llu  Hash: %" PRIx64 "\n",
            totalProfit,
            h);

    orderBookDestroy(b);

    return 0;
}

int
main(int argc, char **argv)
{
    switch(argc) {
        case 2:
            // get orders from input
            return testOrdersInput(strtoull(argv[1], 0, 10));
        case 4:
            // generate orders randomly
            return testOrdersRandom(strtoull(argv[1], 0, 10), 
                    strtoull(argv[2], 0, 10), 
                    atoi(argv[3]));
        default:
            fprintf(stderr, "Usage: %s numBooks OR %s seed orders maxOrder\n", argv[0], argv[0]);
            return 1;
    }
}
