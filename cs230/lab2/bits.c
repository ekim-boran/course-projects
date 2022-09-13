/*
 * CS:APP Data Lab
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */
#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.
     If the puzzle involves unsigned data type, you are also allowed
     to use it.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior on signed integer overflows.
  4. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */

#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
/*
 * allEvenBits - return 1 if all even-numbered bits in word set to 1
 *  where bits are numbered from 0 (least significant) to 31 (most
 *  significant)
 *   Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allEvenBits(int x)
{
  int y = x & (x >> 16);
  int z = y & (y >> 8);
  int t = z & (z >> 4);
  int m = t & (t >> 2);
  return m & 1;
}
/*
 * bitOr - x|y using only ~ and &
 *   Example: bitOr(6, 5) = 7
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitOr(int x, int y)
{
  return ~((~x) & (~y));
}
/*
 * conditional - same as x ? y : z
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */

int conditional(int x, int y, int z)
{
  int r = !x;
  int k = y ^ ((y ^ z) & (~r + 1));
  return k;
}
/*
 * dividePower2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: dividePower2(15,1) = 7, dividePower2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int dividePower2(int x, int n)
{
  // FFFE
  int neg = ((x + ((1 << n) + (~1 + 1))));
  int sign = (x >> 31) & 1; // 1 if negative 0 if positive
  int r = x ^ ((x ^ neg) & (~sign + 1));
  return r >> n;
}
/*
 * floatIsEqual - Compute f == g for floating point arguments f and g.
 *   Both the arguments are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   If either argument is NaN, return 0.
 *   +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 25
 *   Rating: 2
 */
int floatIsEqual(unsigned uf, unsigned ug)
{
  unsigned exp = (0xFF << 23);
  unsigned mantis = ((1 << 23) - 1);
  unsigned signmask = (1 << 31);

  unsigned ufexpt = uf & exp;
  unsigned ufmantis = uf & mantis;
  unsigned ufsign = uf & signmask;

  unsigned ugexpt = ug & exp;
  unsigned ugmantis = ug & mantis;
  unsigned ugsign = ug & signmask;

  unsigned zasd = ~signmask;

  unsigned x = (ufexpt == exp);
  unsigned y = (ugexpt == exp);
  // infinity
  if (x && y && !(ufmantis || ugmantis))
  {
    // if infinity signs must be equal
    return ugsign == ufsign;
  }
  // if not infinity check any of them is nan
  if (x || y)
  {
    return 0;
  }
  // if zero it equals
  if (!((uf & zasd) | (ug & zasd)))
  {
    return 1;
  }
  // normal comparison
  return uf == ug;
}
/*
 * floatUnsigned2Float - Return bit-level equivalent of expression (float) u
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
// it took way too much time for me to solve it 
// keywords guard bit stick bit round bits
// https://www.youtube.com/watch?v=wbxSTxhTmrs time 10:35
unsigned floatUnsigned2Float(unsigned u)
{

  unsigned m, exponent;
  unsigned afterlastbit = 0;
  unsigned lastbit = 0;
  unsigned rest = 0;
  if (u == 0)
  {
    return 0;
  }
  // printf("number %x\n", u);
  exponent = 158;
  while ((u & 0x80000000) == 0)
  {
    exponent--;
    u <<= 1;
  }
  lastbit = (u >> 8) & 1;
  afterlastbit = (u >> 7) & 1;
  rest = u & 127; //

  // printf("%d %d %d %x \n", lastbit, afterlastbit, rest, u);

  // more detailed version
  // if (!afterlastbit)
  //{
  //  m = u >> 8;
  //}
  // else if (afterlastbit && rest != 0)
  //{
  //  m = u >> 8;
  //  m += 1;
  //}
  // else if (afterlastbit && rest == 0 && lastbit)
  //{
  //  m = u >> 8;
  //  m += 1;
  //}
  // else
  //{
  //  m = u >> 8;
  //}
  m = u >> 8;

  if (afterlastbit && rest == 0)
  {
    m += lastbit;
  }
  else if (afterlastbit && rest)
  {
    m += 1;
  }

  return (exponent << 23) | (m & 0x7fffff);
}

/*
 * increment - Compute x+1 without using + and ~
 *   and wrap around to -2147483648 when overflow occurs
 *   Legal ops: ! & ^ | << >>
 *   Max ops: 30
 *   Rating: 4
 */

// koggle-stone adder
// i have no idea how it works
// i copied from 
// https://stackoverflow.com/questions/20494087/is-it-possible-to-write-a-function-adding-two-integers-without-control-flow-and
int increment(int x)
{

  int p = x ^ 1;
  int g = x & 1;

  g |= p & (g << 1);
  p &= p << 1;

  g |= p & (g << 2);
  p &= p << 2;

  g |= p & (g << 4);
  p &= p << 4;

  g |= p & (g << 8);
  p &= p << 8;

  g |= p & (g << 16);

  return x ^ 1 ^ (g << 1);
}
/*
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters
 * '0' to '9') Example: isAsciiDigit(0x35) = 1. isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x)
{
  int a = (x + ~(0x30) + 1);
  int b = (x + ~(0x3a) + 1);

  return (!(a >> 31)) & (b >> 31);
}
/*
 * isPositive - return 1 if x > 0, return 0 otherwise
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int isPositive(int x)
{
  int signf = !((x >> 31) & 1);
  return (signf & !!x);
}
/*
 * isUmax - returns 1 if x is the maximum unsigned integer,
 *     and 0 otherwise
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 4
 *   Rating: 1
 */
unsigned isUmax(unsigned x)
{
  return !(x + 1);
}
/*
 * logicalNeg - implement the ! operator, using all of
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 */
int logicalNeg(int x)
{
  int n = ~x;
  int a = (n >> 16) & n;
  int b = (a >> 8) & a;
  int c = (b >> 4) & b;
  int d = (c >> 2) & c;
  int e = (d >> 1) & d;

  return e & 1;
}
/*
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c)
{
  int mask = 0xFF << (n << 3);
  int masked = x & (~mask);
  int num = c << (n << 3);

  return num | masked;
}
/*
 * unsignedSatAdd - adds two numbers but when overflow occurs,
 *          returns maximum possible value.
 *   Examples: unsignedSatAdd(0x30000000,0x40000000) = 0x70000000
 *             unsignedSatAdd(0x80000000,0x80000000) = 0xFFFFFFFF
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 4
 */
unsigned unsignedSatAdd(unsigned x, unsigned y)
{
  unsigned added = x + y;
  unsigned overflows =
      ((x >> 31) & (y >> 31)) | (((x >> 31) | (y >> 31)) & !(added >> 31));
  unsigned max = (~0);
  int r = !overflows;
  int k = max ^ ((added ^ max) & (~r + 1));
  return k;
}
/*
 * upperBits - pads n upper bits with 1's
 *  You may assume 0 <= n <= 32
 *  Example: upperBits(4) = 0xF0000000
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 12
 *  Rating: 1
 */
int upperBits(int n)
{
  int a = (~0);
  int b = !!n;
  return (a << (32 + (~n + 1))) & (~b + 1);
}
