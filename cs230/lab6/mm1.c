/*
 * mm-naive.c - The fastest, least memory-efficient malloc package.
 *
 * In this naive approach, a block is allocated by simply incrementing
 * the brk pointer.  A block is pure payload. There are no headers or
 * footers.  Blocks are never coalesced or reused. Realloc is
 * implemented directly using mm_malloc and mm_free.
 *
 * NOTE TO STUDENTS: Replace this header comment with your own header
 * comment that gives a high level description of your solution.
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "memlib.h"
#include "mm.h"

#define ALIGNMENT 8
#define ALIGN(size) (((size) + (ALIGNMENT - 1)) & ~0x7)
#define SIZE_T_SIZE (ALIGN(sizeof(size_t)))

#define HSIZE 4
#define FSIZE 4
#define TSIZE HSIZE + FSIZE

char *heap = NULL;

static inline int max(int a, int b) {
  return a > b ? a : b;
}

static inline void writem(char *ptr, int size, int allocated) {
  int *iptr = (int *)ptr;
  *iptr = size | allocated;
}

static inline void w_header_footer(char *header, int bytes, int allocated) {
  writem(header, bytes, allocated);             // write header
  writem(header + bytes - 4, bytes, allocated); // write footer
}

static inline int b_alloc(const char *const header) {
  int allocated = (*(int *)header) & 1;
  return allocated;
}
static inline int b_size(const char *const header) {
  int size =
      (*(int *)header) & ~0x07; // mask out last 3 bits -- they are unused
  return size;
}

static inline char *get_header_from_pointer(void *ptr) {
  char *x = (char *)ptr - HSIZE;
  return x;
}

static inline char *next_block(char *header) {
  int block_size = b_size(header);
  if (block_size == 0) // last block is encountered
  {
    return NULL;
  }
  return header + block_size;
}
static inline char *prev_block(char *header) {
  char *footer = header - 4;
  int block_size = b_size(footer);
  return header - block_size;
}

void print_heap() {
  char *start = heap;

  while (start != NULL) {
    int alloc = b_alloc(start);
    int size = b_size(start);
    printf("%d %d |", alloc, size);
    start = next_block(start);
  }
  printf("\n");
}

char *unify(char *header) {

  char *next = next_block(header);

  int next_allocated = b_alloc(next);
  if (!next_allocated) {
    int size = b_size(next);
    w_header_footer(header, size + b_size(header), 0);
  }

  char *prev = prev_block(header);
  int prev_allocated = b_alloc(prev);
  if (!prev_allocated) {
    int size = b_size(prev);
    w_header_footer(prev, size + b_size(header), 0);
    header = prev;
  }
  return header;
}

char *allocate_space(int bytes) {
  char *start;
  if ((start = mem_sbrk(bytes)) == ((void *)-1)) {
    return NULL;
  }
  char *header = start - 4;
  w_header_footer(header, bytes, 0); // allocate emty block
  writem(header + bytes, 0, 1);      // last block

  char *newheader = unify(header);
  return newheader;
}

// size is requested size + header
int allocate_if_fits(char *header, int size) {
  int block_alloc = b_alloc(header);
  int block_size = b_size(header);

  if (!block_alloc && block_size >= size) {
    int newsize = block_size - size <= 8 ? block_size : size;
    w_header_footer(header, newsize, 1);
    int size_left = block_size - newsize;
    if (size_left > 0) {
      char *next = next_block(header);
      w_header_footer(next, size_left, 0);
    }
    return 1;
  }
  return 0;
}

int mm_init(void) {
  if ((heap = mem_sbrk(16)) == ((void *)-1)) {
    return -1;
  }
  writem(heap, 0, 0); // for alignemt
  w_header_footer(heap + 4, 8, 1);
  writem(heap + 12, 0, 1); // last block
  heap += 4;               // heap pointer points to header of first block
  allocate_space(mem_pagesize() * 2);
  return 0;
}

/*
 * mm_malloc - Allocate a block by incrementing the brk pointer.
 *     Always allocate a block whose size is a multiple of the alignment.
 */
void *mm_malloc(size_t size) {
  if (heap == NULL) {
    mm_init();
  }
  if (size == 0) {
    return NULL;
  }
  int newsize = ALIGN(size + SIZE_T_SIZE);

  char *start = heap;
  while (start != NULL) {
    if (allocate_if_fits(start, newsize)) {
      return start + HSIZE;
    }
    start = next_block(start);
  }
  // not found;
  char *empty_space = allocate_space(
      max(mem_pagesize() * 2, newsize)); // chech if an error encountered
  allocate_if_fits(empty_space, newsize);
  return empty_space + HSIZE;
}

/*
 * mm_free - Freeing a block does nothing.
 */
void mm_free(void *ptr) {
  char *header = get_header_from_pointer(ptr);
  int size = b_size(header);
  w_header_footer(header, size, 0);
  unify(header);
}

/*
 * mm_realloc - Implemented simply in terms of mm_malloc and mm_free
 */
void *mm_realloc(void *ptr, size_t size) {
  char *newptr = mm_malloc(size);
  size_t oldsize = b_size(get_header_from_pointer(ptr));
  if (size < oldsize)
    oldsize = size;
  memcpy(newptr, ptr, oldsize);
  mm_free(ptr);

  return newptr;
}
