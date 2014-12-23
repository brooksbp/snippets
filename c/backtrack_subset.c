#include <stdio.h>

//#define DEBUG

#define MAXCANDIDATES 2
#define NMAX 99
typedef int data;

int finished = 0;

int is_a_solution(int a[], int k, int n) {
  return k == n;
}

void construct_candidates(int a[], int k, int n, int c[], int* ncandidates) {
  c[0] = 1;
  c[1] = 0;
  *ncandidates = 2;
}

void process_solution(int a[], int k, data input) {
  int i;

  printf("{");
  for (i = 1; i <= k; i++) {
    if (a[i] == 1)
#ifndef DEBUG
      printf(" %d", i);
#else
      printf(" x");
#endif
    else
      printf(" -");
  }
  printf(" }\n");
}

void make_move(int a[], int k, data input) {
}
void unmake_move(int a[], int k, data input) {
}

int depth = 0;

void backtrack(int a[], int k, data input) {
  int c[MAXCANDIDATES];
  int ncandidates;
  int i;

  if (is_a_solution(a, k, input)) {
    process_solution(a, k, input);
  } else {
#ifdef DEBUG
    int j;
    for (j = depth; j > 0; j--) printf("  ");
    printf("a={");
    for (j = 1; j <= k; j++) printf(" %d", a[j]);
    printf(" }");
    printf(" k=%d input=%d\n", k, input);
#endif

    k += 1;
    construct_candidates(a, k, input, c, &ncandidates);

    for (i = 0; i < ncandidates; i++) {
      a[k] = c[i];
      make_move(a, k, input);
      depth += 1;
      backtrack(a, k, input);
      depth -= 1;
      unmake_move(a, k, input);
      if (finished)
        return;
    }
  }
}

void generate_subsets(int n) {
  int a[NMAX];

  backtrack(a, 0, n);
}

int main() {
  generate_subsets(4);

  return 0;
}
