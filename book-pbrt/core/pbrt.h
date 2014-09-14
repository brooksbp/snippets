#ifndef __PBRT__
#define __PBRT__

#include <cmath>
//#include <stdlib.h>
//#define _GNU_SOURCE 1
//#include <string.h>
#include <cstring>

#include <algorithm>
using std::min;
using std::max;
using std::swap;

#include <iostream>
using std::cout;
using std::endl;


#include "error.h"

#ifdef DEBUG
/* #define Assert(expr)                            \ */
/*     ((expr) ? (void)0 :                         \ */
/*      printf("Assertion \"%s\" failed in %s:%d", \ */
/*             #expr, __FILE__, __LINE__)) */
// temporary until Severe()
#include <assert.h>
#define Assert(expr) assert(expr)
#else
#define Assert(expr) ((void)0)
#endif

// Global Forward Declarations
class Vector;
class Point;
class Normal;
class Ray;
class RayDifferential;
class BBox;
class Transform;


inline float Lerp(float t, float v1, float v2) {
  return (1.f - t) * v1 + t * v2;
}

#endif // __PBRT__
