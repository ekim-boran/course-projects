#include <stdio.h>
#include <time.h>

void copyij(int src[2048][2048], int dst[2048][2048]) {
  int i, j;
  for (i = 0; i < 2048; i++)
    for (j = 0; j < 2048; j++)
      dst[i][j] = src[i][j];
}

void copyji(int src[2048][2048], int dst[2048][2048]) {
  int i, j;
  for (j = 0; j < 2048; j++)
    for (i = 0; i < 2048; i++)
      dst[i][j] = src[i][j];
}

int src[2048][2048];
int tgt[2048][2048];

int main() {
  clock_t start, end;
  double cpu_time_used;

  start = clock();
  copyij(src, tgt);
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("copyij: %lf seconds\n", cpu_time_used);

  start = clock();
  copyji(src, tgt);
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("copyji: %lf seconds\n", cpu_time_used);

  return 0;
}
