// `CFLAGS = -Wall -Wextra -Wpedantic -Wconversion` in `Makefile` to produce
// warnings

#include <stdio.h>

int main() {
  signed int x = -1;
  unsigned int y = -1;

  x = y;
  y = x;

  return 0;
}
