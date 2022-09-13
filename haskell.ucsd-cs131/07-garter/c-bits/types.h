#include <stddef.h>

#define ZEROBIT       0x00000001
#define CONST_TRUE    0xFFFFFFFF
#define CONST_FALSE   0x7FFFFFFF

// An alias to represent 64-bit raw-memory addresses (pointers)
typedef long* RAddr;

// An alias to represent 64-bit raw-memory values (pointers)
typedef long  RValue;

// An alias to represent indexes (e.g. into a tuple)
typedef int   RIndex;

RValue tuple_at(RAddr base, RIndex i);
RValue print_val(RValue val);
RValue print(RValue val);
RValue equal(RValue val1, RValue val2);
RValue is_number(RValue v);
RValue is_boolean(RValue v);
RValue is_tuple(RValue v);
RAddr  int_addr(RValue);
RValue addr_int(RAddr);

