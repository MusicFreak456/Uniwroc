/*
 * row-major vs. tiled texture queries
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./texture -S 0xdeadc0de -t 65536 -v 0
 * Time elapsed: 1.707234 seconds.
 * $ ./texture -S 0xdeadc0de -t 65536 -v 1
 * Time elapsed: 1.031514 seconds.
 * $ ./texture -S 0xdeadc0de -t 65536 -v 2
 * Time elapsed: 0.935953 seconds.
 */
#include "texture.h"

static inline long index_0(long x, long y) {
  return y * N + x;
}

#define VARIANT 0
#include "texture_impl.h"

/* 
 *  Rozwiązanie z wykorzystaniem krzywej Mortona,
 *  przeplatanie bitów zrobione metodą dziel i 
 *  zwyciężaj.
 */
static inline long index_1(long x, long y) {
  long x_temp = x;
  long y_temp = y;

  long mask1 = 0x0000FFFF0000FFFF;
  long mask2 = 0x00FF00FF00FF00FF;
  long mask3 = 0x0F0F0F0F0F0F0F0F;
  long mask4 = 0x3333333333333333;
  long mask5 = 0x5555555555555555;

  x_temp |= (x_temp << 16);
  y_temp |= (y_temp << 16);
  x_temp &= mask1;
  y_temp &= mask1;
  x_temp |= (x_temp << 8);
  y_temp |= (y_temp << 8);
  x_temp &= mask2;
  y_temp &= mask2;
  x_temp |= (x_temp << 4);
  y_temp |= (y_temp << 4);
  x_temp &= mask3;
  y_temp &= mask3;
  x_temp |= (x_temp << 2);
  y_temp |= (y_temp << 2);
  x_temp &= mask4;
  y_temp &= mask4;
  x_temp |= (x_temp << 1);
  y_temp |= (y_temp << 1);
  x_temp &= mask5;
  y_temp &= mask5;

  return x_temp | (y_temp << 1);
}

#define VARIANT 1
#include "texture_impl.h"
