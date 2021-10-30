/*
 * Binary search with linearly placed tree levels.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./binsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 0
 * Time elapsed: 7.616777 seconds.
 * $ ./binsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 1
 * Time elapsed: 2.884369 seconds.
 */
#include "binsearch.h"

bool binsearch0(T *arr, long size, T x) {
  do {
    size >>= 1;
    T y = arr[size];
    if (y == x)
      return true;
    if (y < x)
      arr += size + 1;
  } while (size > 0);
  return false;
}

/* 
 * Układamy elementy zgodnie ze wskazówką.
 * Realizowane jest  to  poprzez   wzięcie
 * najpierw co (n + 1)/2 elementu, później
 * co (n + 1)/4 elementu  z wyłączeniem co
 * (n + 1)/2 elementu (żeby nie wziąć  dwa
 * razy tego samego) i tak dalej...
 */
void linearize(T *dst, T *src, long size) {
  long prev_stride = size + 1;
  long stride = (size + 1) >> 1;
  long next = stride;
  long i = 0;

  while (i < size) {
    for (; next <= size; next+=stride) {
      if (next % prev_stride != 0) {
        dst[i] = src[next-1];
        i++;
      }
    }
    prev_stride = stride;
    stride >>= 1;
    next = stride;
  }
}

/*
 * Jeśi korzeń jest na i-tej  pozycji  to
 * jego dzieci są  na  pozycjach  2*i + 1 
 * i 2*i + 2, reszta tak jak w zwyczajnym
 * binsearchu.
 */
bool binsearch1(T *arr, long size, T x) {
  int i = 0;

  while (i < size) {
    T y = arr[i];
    if (x == y) return true;
    if (x < y) i += i + 1;
    else i += i + 2;
  }
  
  return false;
}
