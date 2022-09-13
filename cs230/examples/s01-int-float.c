#include <stdio.h>

int main() {
  int x = 40000 * 40000;
  int y = 50000 * 50000;
  printf("40000 * 40000 = %d\n", x);
  printf("50000 * 50000 = %d\n", y);

  float a = (1e20 + -1e20) + 3.14;
  float b = 1e20 + (-1e20 + 3.14);
  printf("(1e20 + -1e20) + 3.14 = %f\n", a);
  printf("1e20 + (-1e20 + 3.14) = %f\n", b);

  return 0;
}
