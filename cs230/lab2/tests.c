/* Testing Code */

#include <limits.h>
#include <math.h>

/* Routines used by floation point test code */

/* Convert from bit level representation to floating point number */
float u2f(unsigned u) {
  union {
    unsigned u;
    float f;
  } a;
  a.u = u;
  return a.f;
}

/* Convert from floating point number to bit-level representation */
unsigned f2u(float f) {
  union {
    unsigned u;
    float f;
  } a;
  a.f = f;
  return a.u;
}
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
int test_allEvenBits(int x) {
  int i;
  for (i = 0; i < 32; i += 2)
    if ((x & (1 << i)) == 0)
      return 0;
  return 1;
}
int test_bitOr(int x, int y) {
  return x | y;
}
int test_conditional(int x, int y, int z) {
  return x ? y : z;
}
int test_dividePower2(int x, int n) {
  int p2n = 1 << n;
  return x / p2n;
}
int test_floatIsEqual(unsigned uf, unsigned ug) {
  float f = u2f(uf);
  float g = u2f(ug);
  return f == g;
}
unsigned test_floatUnsigned2Float(unsigned u) {
  float f = (float)u;
  return f2u(f);
}
int test_increment(int x) {
  if (x == 2147483647)
    return -2147483648;
  return x + 1;
}
int test_isAsciiDigit(int x) {
  return (0x30 <= x) && (x <= 0x39);
}
int test_isPositive(int x) {
  return x > 0;
}
unsigned test_isUmax(unsigned x) {
  return x == 0xFFFFFFFF;
}
int test_logicalNeg(int x) {
  return !x;
}
int test_replaceByte(int x, int n, int c) {
  switch (n) {
  case 0:
    x = (x & 0xFFFFFF00) | c;
    break;
  case 1:
    x = (x & 0xFFFF00FF) | (c << 8);
    break;
  case 2:
    x = (x & 0xFF00FFFF) | (c << 16);
    break;
  default:
    x = (x & 0x00FFFFFF) | (c << 24);
    break;
  }
  return x;
}
unsigned test_unsignedSatAdd(unsigned x, unsigned y) {
  long long xy = (long long)x + (long long)y;
  return xy > 0xFFFFFFFFu ? 0xFFFFFFFFu : xy;
}
int test_upperBits(int x) {
  int result = 0;
  int i;
  for (i = 0; i < x; i++)
    result |= (1 << (31 - i));
  return result;
}
