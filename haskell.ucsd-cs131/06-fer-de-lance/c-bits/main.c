#include <stdio.h>
#include <stdlib.h>

#define ZEROBIT       0x00000001
#define CONST_TRUE    0xFFFFFFFF
#define CONST_FALSE   0x7FFFFFFF
#define HEAP_SIZE     100000

extern long our_code_starts_here()   asm("our_code_starts_here");
extern long print(long val)          asm("print");
extern void error(long code, long v) asm("error");
extern long print_val(long val);

/* Error Codes
 0 : non-number
 1 : non-boolean
 2 : overflow
 3 : non-tuple
 4 : index-too-small
 5 : index-too-small
 */


 void error(long code, long v){
   if (code == 0) {
     fprintf(stderr, "Error: expected a number but got %#010lx\n", v);
   }
   else if (code == 1) {
     fprintf(stderr, "Error: expected a boolean but got %#010lx\n", v);
   }
   else if (code == 2) {
     fprintf(stderr, "Error: arithmetic overflow.");
   }
   else if (code == 3){
     fprintf(stderr, "Error: expected a tuple but got %#010lx\n", v);
   }
   else if (code == 4){
     fprintf(stderr, "Error: tuple index too small.");
   }
   else if (code == 5){
     fprintf(stderr, "Error: tuple index too large.");
   }
   else if (code == 6){
     fprintf(stderr, "Error: expected a closure but got %#010lx\n", v);
   }
   else if (code == 7){
     fprintf(stderr, "Dynamic Error: function call arity mismatch");
   }
   exit(1);
 }

long is_number(long v){
  return ((v & 1) == 0);
}

long is_boolean(long v){
  return ((v & CONST_FALSE) == CONST_FALSE);
}

long is_tuple(long v){
  return ((v & 7) == 1);
}

long is_closure(long v){
  return ((v & 7) == 5);
}

void print_number(long val){
  printf("%ld", val >> 1);
}

void print_boolean(long val){
  if (val == CONST_TRUE)
    printf("true");
  else // if (val == CONST_FALSE)
    printf("false");
}

void print_tuple(long val){
  long *base = (long *) (val - 1);
  long size = (*base) >> 1;
  printf("(");
  print_val(base[1]);
  for (long i = 2; i <= size; i++){
    printf(", ");
    print_val(base[i]);
  }
  printf(")");
}

long print_val(long val) {
  if (is_number(val))
    print_number(val);
  else if (is_boolean(val))
    print_boolean(val);
  else if (is_tuple(val))
    print_tuple(val);
  else if (is_closure(val))
    printf("<function>");
  else
    printf("Unknown value: %#010lx\n", val);
  return val;
}

long print(long val){
  print_val(val);
  printf("\n");
  return val;
}

int main(int argc, char** argv) {
  int* HEAP = calloc(HEAP_SIZE, sizeof (int));
  long result = our_code_starts_here(HEAP);
  print(result);
  return 0;
}
