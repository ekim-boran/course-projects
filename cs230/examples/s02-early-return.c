#include <assert.h>
#include <stdio.h>

int foo(int x) {
  printf("foo(%d)\n", x);
  return x;
}

int main() {
  assert((0 && foo(42)) == 0);
  assert((0 || foo(43)) == 1);
  assert((1 && foo(44)) == 1);
  assert((1 || foo(45)) == 1);

  return 0;
}
