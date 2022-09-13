#include <stdio.h>
#include <stdlib.h>

#define TYPE_MASK 0x00000001
#define CONST_TRUE 0x80000001
#define CONST_FALSE 0x00000001

extern long our_code_starts_here() asm("our_code_starts_here");

void error(int error_code)
{
  if (error_code == 0)
  {
    fprintf(stderr, "Error: expected a number");
  }
  else if (error_code == 1)
  {
    fprintf(stderr, "Error: expected a boolean");
  }
  else
  {
    fprintf(stderr, "Error: arithmetic overflow");
  }

  exit(1);
}

long print(long val)
{
  if ((val & 1) == 1)
  {
    if (val == 0x80000001)
    {
      printf("%s\n", "true");
    }
    else
    {
      printf("%s\n", "false");
    }
  }
  else
  {
    printf("%ld\n", val >> 1);
  }
  //  printf("Unknown value: %#010lx\n", val >> 1);

  return val;
}

int main(int argc, char **argv)
{
  long result = our_code_starts_here();
  print(result);
  return 0;
}
