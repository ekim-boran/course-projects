#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/queue.h>
#include "types.h"
#include "gc.h"

#define USE_GC 1 // switch this to 1 to use your gc(...)

extern RValue our_code_starts_here() asm("our_code_starts_here");
extern void error(RValue code, RValue v) asm("error");
extern RValue print_val(RValue val);
extern RValue print(RValue val) asm("print");
extern RValue equal(RValue val1, RValue val2) asm("equal");
extern RAddr try_gc(RAddr alloc_ptr, int amount_needed, RAddr first_frame, RAddr stack_top) asm("try_gc");

extern RAddr HEAP asm("HEAP");
extern RAddr HEAP_END asm("HEAP_END");
extern RAddr STACK_BOTTOM asm("STACK_BOTTOM");

size_t HEAP_SIZE;
RAddr HEAP_PTR;
RAddr HEAP;
RAddr HEAP_END;
RAddr STACK_BOTTOM;

/******************************************************************************/
/** ERROR handling ************************************************************/
/******************************************************************************/

/* Error Codes
 0 : non-number
 1 : non-boolean
 2 : overflow
 3 : non-tuple
 4 : index-too-small
 5 : index-too-small
 */

void error(long code, long v)
{
  if (code == 0)
  {
    fprintf(stderr, "Error: expected a number but got %#010lx\n", v);
  }
  else if (code == 1)
  {
    fprintf(stderr, "Error: expected a boolean but got %#010lx\n", v);
  }
  else if (code == 2)
  {
    fprintf(stderr, "Error: arithmetic overflow.");
  }
  else if (code == 3)
  {
    fprintf(stderr, "Error: expected a tuple but got %#010lx\n", v);
  }
  else if (code == 4)
  {
    fprintf(stderr, "Error: tuple index too small.");
  }
  else if (code == 5)
  {
    fprintf(stderr, "Error: tuple index too large.");
  }
  exit(1);
}

/******************************************************************************/
/** GARBAGE COLLECTION ********************************************************/
/******************************************************************************/

/* Try to clean up space in memory by calling gc.

  You do not need to edit this function.

  Arguments:

    - alloc_ptr: The current value of ESI (where the next value would be
      allocated without GC)
    - bytes_needed: The number of bytes that the runtime is trying to allocate
    - first_frame: The current value of EBP (for tracking stack information)
    - stack_top: The current value of ESP (for tracking stack information)

  Returns:

    The new value for ESI, for the runtime to start using as the allocation
    point.  Must be set to a location that provides enough room to fit
    bytes_allocated more bytes in the given heap space

*/
long *try_gc(RAddr alloc_ptr, RIndex bytes_needed, RAddr first_frame, RAddr stack_top)
{

  RAddr new_heap_ptr;

  if (DEBUG)
  {
    fprintf(stderr, "====================================\n");
    fprintf(stderr, "====================================\n");
    fprintf(stderr, "====================================\n");
    fprintf(stderr, "bytes needed : %d\n", bytes_needed);
    fprintf(stderr, "HEAP         : %p\n", HEAP);
    fprintf(stderr, "HEAP END     : %p\n", HEAP_END);
    fprintf(stderr, "alloc ptr    : %p\n", alloc_ptr);
    fprintf(stderr, "STACK BOTTOM : %p\n", STACK_BOTTOM);
    fprintf(stderr, "first frame  : %p\n", first_frame);
    fprintf(stderr, "stack top    : %p\n", stack_top);
    fprintf(stderr, "\n");
  }

  //if (HEAP == alloc_ptr)
  //{
  //  fprintf(stderr, "Out of memory: Allocation of %d words too large for %ld-word heap\n", bytes_needed / 8, (HEAP_END - HEAP));
  //  if (DEBUG)
  //    fprintf(stderr, "FREE at %p \n", HEAP);
  //  free(HEAP_PTR);
  //  exit(10);
  //}
  if (USE_GC)
  {
    // This uses your GC; toggle USE_GC when confident!
    new_heap_ptr = gc(STACK_BOTTOM, stack_top, first_frame, HEAP, HEAP_END);
  }
  else
  {
    // This skips GC and has the effect of just keeping R15 where it is.
    new_heap_ptr = alloc_ptr;
  }

  if (DEBUG)
    fprintf(stderr, "new esi      : %p\n", new_heap_ptr);

  if ((new_heap_ptr + (bytes_needed / 8)) > HEAP_END)
  {
    fprintf(stderr, "\nOut of memory: needed %d words, but only %ld remain after collection!\n\n", bytes_needed / 8, (HEAP_END - new_heap_ptr));
    if (DEBUG)
      fprintf(stderr, "FREE at %p \n", HEAP);
    free(HEAP_PTR);
    exit(9);
  }
  else
  {
    return new_heap_ptr;
  }
}

// jostle `ptr` to ensure it is 8-byte aligned (no need to do it in your ASM)
RAddr align8(RAddr ptr)
{
  RValue index = (RValue)ptr;
  if ((index & 0x7) == 0)
    return ptr;
  index += 8;
  index &= 0xFFFFFFFFFFFFFFF8;
  return (RAddr)index;
}

// set HEAP_SIZE using args or default
void setHeapSize(int argc, char **argv)
{
  if (argc > 1)
  {
    HEAP_SIZE = atoi(argv[1]);
  }
  else
  {
    HEAP_SIZE = 20;
  }
}

// requires that HEAP_SIZE has been set by setHeapSize
void initHeap()
{
  if (DEBUG)
    fprintf(stderr, "Heap Size = %ld\n", HEAP_SIZE);

  // allocate HEAP
  HEAP = calloc(HEAP_SIZE, sizeof(long));

  // abort if no mem
  if (HEAP == NULL)
  {
    fprintf(stderr, "HEAP is NULL !!!\n");
    exit(1);
  }

  // set heap end
  HEAP_END = HEAP + HEAP_SIZE;

  // clear
  for (int i = 0; i < HEAP_SIZE; i++)
  {
    HEAP[i] = 0;
  }

  if (DEBUG)
    fprintf(stderr, "ALLOC-0: %p, %p \n", HEAP, HEAP_END);

  // 8-byte align
  HEAP = align8(HEAP);

  if (DEBUG)
    fprintf(stderr, "ALLOC-1: %p \n", HEAP);

  // store original to properly 'free'
  HEAP_PTR = HEAP;
}

/******************************************************************************/
/** Top-level "main" **********************************************************/
/******************************************************************************/

int main(int argc, char **argv)
{
  setHeapSize(argc, argv);
  initHeap();
  RValue result = our_code_starts_here(HEAP);
  print(result);
  return 0;
}
