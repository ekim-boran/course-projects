#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "types.h"

extern RValue print_val(RValue val);
extern RValue print(RValue val) asm("print");
extern RValue equal(RValue val1, RValue val2) asm("equal");
extern RValue is_number(RValue v);
extern RValue is_boolean(RValue v);
extern RValue is_tuple(RValue v);

/******************************************************************************/
/** Convert a raw 'int' into a pointer ****************************************/
/******************************************************************************/

// take an int 'val' ending with '001' and return it as a pointer
RAddr int_addr(RValue val)
{
  return (long *)(val - 1);
}

// assumes 8 byte aligned 'addr' (last 3 bits are 0)
RValue addr_int(RAddr addr)
{
  long v = (long)addr;
  return v + 1; // set the last bit to 1
}

/******************************************************************************/
/** EQUALITY test *************************************************************/
/******************************************************************************/

RValue equal(RValue val1, RValue val2)
{
  if (val1 == val2)
  {
    return CONST_TRUE;
  }
  else
  {
    return CONST_FALSE;
  }
}

/******************************************************************************/
/** PRINTING ******************************************************************/
/******************************************************************************/

long is_number(RValue v)
{
  return ((v & 1) == 0);
}

long is_boolean(RValue v)
{
  return ((v & CONST_FALSE) == CONST_FALSE);
}

long is_tuple(RValue v)
{
  return ((v & 7) == 1);
}

void print_number(RValue val)
{
  printf("%ld", val >> 1);
}

void print_boolean(RValue val)
{
  if (val == CONST_TRUE)
    printf("true");
  else // if (val == CONST_FALSE)
    printf("false");
}

RValue tuple_at(RAddr base, RIndex i)
{
  return base[i + 2];
}

RIndex tuple_size(RAddr base)
{
  return (*base) >> 1;
}

void print_tuple(RValue val)
{
  RAddr base = int_addr(val);
  RIndex size = tuple_size(base);
  // printf("<tuple:%d>", size);
  printf("(");
  print_val(tuple_at(base, 0));
  for (RIndex i = 1; i < size; i++)
  {
    printf(", ");
    print_val(tuple_at(base, i));
  }
  printf(")");
}

RValue print_val(RValue val)
{
  if (is_number(val))
    print_number(val);
  else if (is_boolean(val))
    print_boolean(val);
  else if (is_tuple(val))
    print_tuple(val);
  else
    printf("Unknown value: %#010lx", val);
  return val;
}

RValue print(RValue val)
{
  fprintf(stderr, "hobarak...., %ld\n", val);

  print_val(val);
  printf("\n");
  return val;
}
