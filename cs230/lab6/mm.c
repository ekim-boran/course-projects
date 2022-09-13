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
void print_heap();

#define ALIGNMENT 8
#define ALIGN(size) (((size) + (ALIGNMENT - 1)) & ~0x7)
#define SIZE_T_SIZE (ALIGN(16))

#define HSIZE 4
#define FSIZE 4
#define TSIZE HSIZE + FSIZE

char *heap = NULL;
char *free_list = NULL;

int pagesize = 0;
static inline int max(int a, int b)
{

  return a > b ? a : b; //(b & (~0xFFF)) + 0x1000;
}

static inline void writem(char *ptr, int size, int allocated)
{
  int *iptr = (int *)ptr;
  *iptr = size | allocated;
}

static inline void write_ptr(char *ptr, char *x)
{
  int *iptr = (int *)ptr;
  *iptr = (int)x;
}

static inline void w_header_footer(char *header, int bytes, int allocated)
{
  writem(header, bytes, allocated);             // write header
  writem(header + bytes - 4, bytes, allocated); // write footer
}

static inline int b_alloc(char *header)
{
  int allocated = (*(int *)header) & 1;
  return allocated;
}
static inline int b_size(char *header)
{
  int size =
      (*(int *)header) & ~0x07; // mask out last 3 bits -- they are unused
  return size;
}

static inline char *get_header_from_pointer(void *ptr)
{
  char *x = (char *)ptr - HSIZE;
  return x;
}

static inline char *next_block(char *header)
{
  int block_size = b_size(header);
  if (block_size == 0) // last block is encountered
  {
    return NULL;
  }
  return header + block_size;
}
static inline char *prev_block(char *header)
{
  char *footer = header - 4;
  int block_size = b_size(footer);
  return header - block_size;
}

static inline char *next_ptr(char *header)
{
  char *size = *((char **)(header + 8));
  return size;
}

static inline char *prev_ptr(char *header)
{
  char *size = *((char **)(header + 4));
  return size;
}

static inline void write_prev(char *header, char *ptr)
{
  write_ptr(header + 4, ptr);
}
static inline void write_next(char *header, char *ptr)
{
  write_ptr(header + 8, ptr);
}

static inline void insert_between(char *before, char *after, char *new_node)
{
  write_prev(new_node, before);
  write_next(new_node, after);
  write_next(before, new_node);
  write_prev(after, new_node);
}

static inline void remove_node(char *ptr)
{
  char *next_old = next_ptr(ptr);
  char *prev_old = prev_ptr(ptr);
  write_prev(next_old, prev_old);
  write_next(prev_old, next_old);
}

// coelesce if you can
// if not add to free list
char *mark_free(char *ptr)
{

  char *next = next_block(ptr);
  int next_allocated = b_alloc(next);
  char *prev = prev_block(ptr);
  int prev_allocated = b_alloc(prev);

  if (!next_allocated && prev_allocated)
  {
    int size = b_size(next);
    insert_between(prev_ptr(next), next_ptr(next), ptr);
    w_header_footer(ptr, size + b_size(ptr), 0);
    return ptr;
  }
  if (next_allocated && !prev_allocated)
  {
    int size = b_size(prev);
    w_header_footer(prev, size + b_size(ptr), 0);
    return prev;
  }
  if (!next_allocated && !prev_allocated)
  {
    int size1 = b_size(prev);
    int size2 = b_size(next);
    remove_node(next);
    w_header_footer(prev, size1 + size2 + b_size(ptr), 0);
    return prev;
  }
  // else both allocated -- insert to head of list

  insert_between(free_list, next_ptr(free_list), ptr);
  return ptr;
}

// size is requested size + header
char *find_free_block(int size)
{
  char *free_block = next_ptr(free_list);
  while (!b_alloc(free_block)) // because the list is circular eventually it //
                               // reaches start node which returns false
  {
    // printf("%x %x\n", (int)free_list, (int)free_block);
    if (b_size(free_block) >= size)
    {
      return free_block;
    }
    free_block = next_ptr(free_block);
  }
  return NULL;
}

int place(char *ptr, int size)
{
  int block_size = b_size(ptr);
  char *next_old = next_ptr(ptr);
  char *prev_old = prev_ptr(ptr);

  int newsize = block_size - size <= 16 ? block_size : size;
  w_header_footer(ptr, newsize, 1);
  int size_left = block_size - newsize;
  if (size_left > 0)
  {
    char *next = next_block(ptr);
    w_header_footer(next, size_left, 0);
    insert_between(prev_old, next_old, next);
  }
  else
  {
    write_prev(next_old, prev_old);
    write_next(prev_old, next_old);
  }
  return newsize;
  // modify pointers
  // mark some space as
}

char *allocate_space(int bytes)
{
  char *start;
  if ((start = mem_sbrk(bytes)) == ((void *)-1))
  {
    return NULL;
  }
  char *header = get_header_from_pointer(start);
  w_header_footer(header, bytes, 0); // allocate emty block
  writem(header + bytes, 0, 1);      // last block
  return mark_free(header);
}

int mm_init(void)
{
  if ((heap = mem_sbrk(24)) == ((void *)-1))
  {
    return -1;
  }
  pagesize = mem_pagesize();
  writem(heap, 0, 0); // for alignemt
  heap += 4;
  w_header_footer(heap, 16, 1);

  write_prev(heap, heap);
  write_next(heap, heap);  // right ptr
  writem(heap + 20, 0, 1); // last block
  free_list = heap;

  allocate_space(pagesize);
  return 0;
}

void *mm_malloc(size_t size)
{
  //printf("before malloc %d\n", size);
  //print_heap();

  if (heap == NULL)
  {
    mm_init();
  }
  if (size == 0)
  {
    return NULL;
  }
  int newsize = ALIGN(size + SIZE_T_SIZE);
  char *start = find_free_block(newsize);
  if (start == NULL)
  {
    start = allocate_space(max(pagesize, newsize));
  }
  place(start, newsize);

  //printf("after malloc %d\n", size);
  //print_heap();
  return start + HSIZE;
}

/*
 * mm_free - Freeing a block does nothing.
 */
void mm_free(void *ptr)
{
  char *header = get_header_from_pointer(ptr);
  int size = b_size(header);
  w_header_footer(header, size, 0);
  mark_free(header);
}

/*
 * mm_realloc - Implemented simply in terms of mm_malloc and mm_free
 */
void *mm_realloc(void *ptr, size_t size)
{
  size_t newsize = ALIGN(size + SIZE_T_SIZE);
  char *header = get_header_from_pointer(ptr);
  size_t oldsize = b_size(header);
  //printf("realloc: old: %d new: %d\n", (int)oldsize, (int)newsize);

  int magic = 32;
  //  reduce the size if requested amount is magic bytes smaller than original amount

  if (newsize <= oldsize - magic)
  {
    w_header_footer(header, newsize, 1);
    char *next = next_block(header);
    w_header_footer(next, oldsize - newsize, 0);
    mark_free(next);
    //printf(" op : small \n");
    return ptr;
  }
  else if (newsize <= oldsize)
  {
    //printf(" op : same \n");

    return ptr;
  }

  char *next = next_block(header);

  // there is enough space

  if (!b_alloc(next) && oldsize + b_size(next) >= newsize)
  {
    int newspace = place(next, newsize - oldsize); // place handles linked list related operations
    w_header_footer(header, oldsize + newspace, 1);
    // printf(" op : enough space  \n");

    return ptr;
  }

  char *newptr = mm_malloc(size);
  //printf(" op : relocate \n");

  memcpy(newptr, ptr, oldsize - 8); // without header / footer
  mm_free(ptr);
  return newptr;
}

void print_heap()
{
  char *start = heap;
  printf("heap %x %x %x\n", (int)heap, (int)prev_ptr(start), (int)next_ptr(start));

  while (start != NULL)
  {

    int alloc = b_alloc(start);
    int size = b_size(start);
    if (!alloc)
    {
      printf("%x %d %x %x|", (int)start, size, (int)prev_ptr(start),
             (int)next_ptr(start));
    }
    else
    {
      printf("%x  %d %d|", (int)start, alloc, size);
    }
    start = next_block(start);
  }
  printf("\n");
}