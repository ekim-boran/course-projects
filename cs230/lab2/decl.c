#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#define TMin INT_MIN
#define TMax INT_MAX

#include "bits.h"
#include "btest.h"

test_rec test_set[] = {
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
    {"allEvenBits",
     (funct_t)allEvenBits,
     (funct_t)test_allEvenBits,
     1,
     "! ~ & ^ | + << >>",
     12,
     2,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"bitOr",
     (funct_t)bitOr,
     (funct_t)test_bitOr,
     2,
     "& ~",
     8,
     1,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"conditional",
     (funct_t)conditional,
     (funct_t)test_conditional,
     3,
     "! ~ & ^ | << >>",
     16,
     3,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"dividePower2",
     (funct_t)dividePower2,
     (funct_t)test_dividePower2,
     2,
     "! ~ & ^ | + << >>",
     15,
     2,
     {{TMin, TMax}, {0, 30}, {TMin, TMax}}},
    {"floatIsEqual",
     (funct_t)floatIsEqual,
     (funct_t)test_floatIsEqual,
     2,
     "$",
     25,
     2,
     {{1, 1}, {1, 1}, {1, 1}}},
    {"floatUnsigned2Float",
     (funct_t)floatUnsigned2Float,
     (funct_t)test_floatUnsigned2Float,
     1,
     "$",
     30,
     4,
     {{1, 1}, {1, 1}, {1, 1}}},
    {"increment",
     (funct_t)increment,
     (funct_t)test_increment,
     1,
     "! & ^ | << >>",
     30,
     4,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"isAsciiDigit",
     (funct_t)isAsciiDigit,
     (funct_t)test_isAsciiDigit,
     1,
     "! ~ & ^ | + << >>",
     15,
     3,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"isPositive",
     (funct_t)isPositive,
     (funct_t)test_isPositive,
     1,
     "! ~ & ^ | + << >>",
     8,
     2,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"isUmax",
     (funct_t)isUmax,
     (funct_t)test_isUmax,
     1,
     "! ~ & ^ | +",
     4,
     1,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"logicalNeg",
     (funct_t)logicalNeg,
     (funct_t)test_logicalNeg,
     1,
     "~ & ^ | + << >>",
     12,
     4,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"replaceByte",
     (funct_t)replaceByte,
     (funct_t)test_replaceByte,
     3,
     "! ~ & ^ | + << >>",
     10,
     3,
     {{TMin, TMax}, {0, 3}, {0, 255}}},
    {"unsignedSatAdd",
     (funct_t)unsignedSatAdd,
     (funct_t)test_unsignedSatAdd,
     2,
     "! ~ & ^ | + << >>",
     25,
     4,
     {{TMin, TMax}, {TMin, TMax}, {TMin, TMax}}},
    {"upperBits",
     (funct_t)upperBits,
     (funct_t)test_upperBits,
     1,
     "! ~ & ^ | + << >>",
     12,
     1,
     {{0, 32}, {TMin, TMax}, {TMin, TMax}}},
    {"", NULL, NULL, 0, "", 0, 0, {{0, 0}, {0, 0}, {0, 0}}}};
