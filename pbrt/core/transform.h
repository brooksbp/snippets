#ifndef __TRANSFORM__
#define __TRANSFORM__

#include "pbrt.h"

struct Mat4 {
  float m[4][4];
  
  Mat4() {
    m[0][0] = m[1][1] = m[2][2] = m[3][3] = 1.f;
    m[0][1] = m[0][2] = m[0][3] =
    m[1][0] = m[1][2] = m[1][3] =
    m[2][0] = m[2][1] = m[2][3] =
    m[3][0] = m[3][1] = m[3][2] = 0.f;
  }
  Mat4(float mat[4][4]);
  Mat4(float t00, float t01, float t02, float t03,
       float t10, float t11, float t12, float t13,
       float t20, float t21, float t22, float t23,
       float t30, float t31, float t32, float t33);

  bool operator==(const Mat4 &m2) const {
    for (int i = 0; i < 4; i++)
      for (int j = 0; j < 4; j++)
        if (m[i][j] != m2.m[i][j])
          return false;
    return true;
  }
  bool operator!=(const Mat4 &m2) const {
    for (int i = 0; i < 4; i++)
      for (int j = 0; j < 4; j++)
        if (m[i][j] != m2.m[i][j])
          return true;
    return false;
  }

  friend Mat4 Transpose(const Mat4 &);

  void Print(FILE *f) const {
    fprintf(f, "[ ");
    for (int i = 0; i < 4; i++) {
      fprintf(f, "  [ ");
      for (int j = 0; j < 4; j++) {
        fprintf(f, "%f", m[i][j]);
        if (j != 3)
          fprintf(f, ", ");
      }
      fprintf(f, " ]\n");
    }
    fprintf(f, " ] ");
  }

  static Mat4 Mul(const Mat4 &m1, const Mat4 &m2) {
    Mat4 r;
    for (int i = 0; i < 4; i++)
      for (int j = 0; j < 4; j++)
        r.m[i][j] = m1.m[i][0] * m2.m[0][j] +
                    m1.m[i][1] * m2.m[1][j] +
                    m1.m[i][2] * m2.m[2][j] +
                    m1.m[i][3] * m2.m[3][j];
    return r;
  }

  friend Mat4 Inverse(const Mat4 &);
  
};

// 128 bytes per Transform. Millions of shapes, but a few
// thousand unique transforms... use transform cache and
// have pointer to Transform in shape. Transform must be
// immutable since one shape shouldn't alter another
// shape's transform. Possible to duplicate Transform and
// mutate in cache...

class Transform {
public:

  Transform() { }
  Transform(const float mat[4][4]) {
    m = Mat4(mat[0][0], mat[0][1], mat[0][2], mat[0][3],
             mat[1][0], mat[1][1], mat[1][2], mat[1][3],
             mat[2][0], mat[2][1], mat[2][2], mat[2][3],
             mat[3][0], mat[3][1], mat[3][2], mat[3][3]);
    mInv = Inverse(m);
  }
  Transform(const Mat4 &mat)
      : m(mat), mInv(Inverse(mat)) {
  }
  Transform(const Mat4 &mat, const Mat4 &minv)
      : m(mat), mInv(minv) {
  }

  friend Transform Inverse(const Transform &t) {
    return Transform(t.mInv, t.m);
  }

  bool operator==(const Transform &t) const {
    return t.m == m && t.mInv == mInv;
  }
  bool operator!=(const Transform &t) const {
    return t.m != m || t.mInv != mInv;
  }

  bool IsIdentity() const {
    return (m.m[0][0] == 1.f && m.m[0][1] == 0.f &&
            m.m[0][2] == 0.f && m.m[0][3] == 0.f &&
            m.m[1][0] == 0.f && m.m[1][1] == 1.f &&
            m.m[1][2] == 0.f && m.m[1][3] == 0.f &&
            m.m[2][0] == 0.f && m.m[2][1] == 0.f &&
            m.m[2][2] == 1.f && m.m[2][3] == 0.f &&
            m.m[3][0] == 0.f && m.m[3][1] == 0.f &&
            m.m[3][2] == 0.f && m.m[3][3] == 1.f);
  }
  
private:
  Mat4 m, mInv;
};

#endif // __TRANSFORM__
