#ifndef __GEOMETRY__
#define __GEOMETRY__

#include "pbrt.h"

class Vector {
public:
  float x, y, z;

  Vector() {
    x = y = z = 0.f;
  }
  Vector(float xx, float yy, float zz)
      : x(xx), y(yy), z(zz) {
    Assert(!HasNaNs());
  }
  bool HasNaNs() const { return isnan(x) || isnan(y) || isnan(z); }
  explicit Vector(const Normal &n);

  friend std::ostream& operator<<(std::ostream& os, const Vector& a) {
    os << "Vector<" << a.x << "," << a.y << "," << a.z << ">";
    return os;
  }

  Vector operator+(const Vector &a) const {
    return Vector(x + a.x, y + a.y, z + a.z);
  }
  Vector& operator+=(const Vector &a) {
    x += a.x; y += a.y; z += a.z;
    return *this;
  }
  Vector operator-(const Vector &a) const {
    return Vector(x - a.x, y - a.y, z - a.z);
  }
  Vector& operator-=(const Vector &a) {
    x -= a.x; y -= a.y; z -= a.z;
    return *this;
  }
  Vector operator*(float f) const {
    return Vector(f * x, f * y, f * z);
  }
  Vector& operator*=(float f) {
    x *= f; y *= f; z *= f;
    return *this;
  }
  Vector operator/(float f) const {
    Assert(f != 0);
    float inv = 1.f / f;
    return Vector(x * inv, y * inv, z * inv);
  }
  Vector& operator/=(float f) {
    Assert(f != 0);
    float inv = 1.f / f;
    x *= inv; y *= inv; z *= inv;
    return *this;
  }
  Vector operator-() const {
    return Vector(-x, -y, -z);
  }
  float operator[](int i) const {
    Assert(i >= 0 && i <= 2);
    return (&x)[i];
  }
  float& operator[](int i) {
    Assert(i >= 0 && i <= 2);
    return (&x)[i];
  }

  float LengthSquared() const {
    return x*x + y*y + z*z;
  }
  float Length() const {
    return sqrtf(LengthSquared());
  }

};

inline Vector operator*(float f, const Vector &a) {
  return a * f;
}

inline float Dot(const Vector &a, const Vector &b) {
  return a.x * b.x + a.y * b.y + a.z *b.z;
}

inline float AbsDot(const Vector &a, const Vector &b) {
  return fabsf(Dot(a, b));
}

inline Vector Cross(const Vector &a, const Vector &b) {
  return Vector((a.y * b.z) - (a.z * b.y),
                (a.z * b.x) - (a.x * b.z),
                (a.x * b.y) - (a.y * b.x));
}

inline Vector Normalize(const Vector &a) {
  return a / a.Length();
}

inline void CoordinateSystem(const Vector &v1, Vector *v2, Vector *v3) {
  if (fabsf(v1.x) > fabsf(v1.y)) {
    float invLen = 1.f / sqrtf(v1.x * v1.x + v1.z * v1.z);
    *v2 = Vector(-v1.z * invLen, 0.f, v1.x * invLen);
  } else {
    float invLen = 1.f / sqrtf(v1.y * v1.y + v1.z * v1.z);
    *v2 = Vector(0.f, v1.z * invLen, -v1.y * invLen);
  }
  *v3 = Cross(v1, *v2);
}


class Point {
public:
  float x, y, z;

  Point() {
    x = y = z = 0.f;
  }
  Point(float xx, float yy, float zz)
    : x(xx), y(yy), z(zz) {
  }

  Point operator+(const Vector &v) const {
    return Point(x + v.x, y + v.y, z + v.z);
  }
  Point& operator+=(const Vector &v) {
    x += v.x; y += v.y; z += v.z;
    return *this;
  }
  Point operator+(const Point &p) const {
    return Point(x + p.x, y + p.y, z + p.z);
  }
  Point& operator+=(const Point &p) {
    x += p.x; y += p.y; z += p.z;
    return *this;
  }

  // v = p' - p
  Vector operator-(const Point &p) const {
    return Vector(x - p.x, y - p.y, z - p.z);
  }
  Point operator-(const Vector &v) const {
    return Point(x - v.x, y - v.y, z - v.z);
  }
  Point& operator-=(const Vector &v) {
    x -= v.x; y -= v.y; z -= v.z;
    return *this;
  }

  Point operator*(float f) const {
    return Point(f*x, f*y, f*z);
  }
  Point &operator*=(float f) {
    x *= f; y *= f; z *= f;
    return *this;
  }
  
};

inline Point operator*(float f, const Point &p) {
  //Assert(!p.HasNaNs());
  return p*f;
}

inline float Distance(const Point &p1, const Point &p2) {
  return (p1 - p2).Length();
}

inline float DistanceSquared(const Point &p1, const Point &p2) {
  return (p1 - p2).LengthSquared();
}


class Normal {
public:
  float x, y, z;

  Normal() {
    x = y = z = 0.f;
  }
  Normal(float xx, float yy, float zz)
    : x(xx), y(yy), z(zz) {
    Assert(!HasNaNs());
  }
  bool HasNaNs() const { return isnan(x) || isnan(y) || isnan(z); }

  explicit Normal(const Vector &v)
      : x(v.x), y(v.y), z(v.z) {
  }

  Normal operator-() const {
    return Normal(-x, -y, -z);
  }

};

// Normal fwd decl doesn't seem to help
// if moved up near Vector definition..
inline Vector::Vector(const Normal &n)
              : x(n.x), y(n.y), z(n.z) {
}

inline float Dot(const Normal &n1, const Vector &v2) {
  Assert(!n1.HasNaNs() && !v2.HasNaNs());
  return n1.x * v2.x + n1.y * v2.y + n1.z * v2.z;
}

inline Normal Faceforward(const Normal &n, const Vector &v) {
  return (Dot(n, v) < 0.f) ? -n : n;
}


class Ray {
public:
  Point o;
  Vector d;
  mutable float mint, maxt;
  float time;
  int depth;

  Ray() : mint(0.f), maxt(INFINITY), time(0.f), depth(0) { }
  Ray(const Point &origin, const Vector &direction,
      float start, float end = INFINITY, float t = 0.f, int d = 0)
      : o(origin), d(direction), mint(start), maxt(end), time(t), depth(d) { }
  Ray(const Point &origin, const Vector &direction, const Ray &parent,
      float start, float end = INFINITY)
      : o(origin), d(direction), mint(start), maxt(end),
      time(parent.time), depth(parent.depth+1) { }

  // Parametric form a function of t. To get a point
  // along a ray:
  //   Ray r(Point(0,0,0), Vector(1,2,3));
  //   Point p = r(1.7);
  Point operator()(float t) const {
    return o + d * t;
  }

};


class RayDifferential : public Ray {
public:
  bool hasDifferentials;
  Point rxOrigin, ryOrigin;
  Vector rxDirection, ryDirection;
  
  RayDifferential() { hasDifferentials = false; }
  RayDifferential(const Point &org, const Vector &dir, float start,
                  float end = INFINITY, float t = 0.f, int d = 0)
      : Ray(org, dir, start, end, t, d) {
    hasDifferentials = false;
  }
  RayDifferential(const Point &org, const Vector &dir, const Ray &parent,
                  float start, float end = INFINITY)
      : Ray(org, dir, start, end, parent.time, parent.depth+1) {
    hasDifferentials = false;
  }
  explicit RayDifferential(const Ray &ray) : Ray(ray) {
    hasDifferentials = false;
  }

  void ScaleDifferentials(float s) {
    rxOrigin = o + (rxOrigin - o) * s;
    ryOrigin = o + (ryOrigin - o) * s;
    rxDirection = d + (rxDirection - d) * s;
    ryDirection = d + (ryDirection - d) * s;
  }
  
};

// axis-aligned bounding box (AABB)
class BBox {
public:
  Point pMin, pMax;

  BBox() {
    pMin = Point( INFINITY,  INFINITY,  INFINITY);
    pMax = Point(-INFINITY, -INFINITY, -INFINITY);
  }
  BBox(const Point &p)
      : pMin(p), pMax(p) { }
  BBox(const Point &p1, const Point &p2) {
    pMin = Point(min(p1.x, p2.x), min(p1.y, p2.y), min(p1.z, p2.z));
    pMin = Point(max(p1.x, p2.x), max(p1.y, p2.y), max(p1.z, p2.z));
  }

  friend BBox Union(const BBox &b, const Point &p);
  friend BBox Union(const BBox &b, const BBox &b2);

  bool Overlaps(const BBox &b) const {
    bool x = (pMax.x >= b.pMin.x) && (pMin.x <= b.pMax.x);
    bool y = (pMax.y >= b.pMin.y) && (pMin.y <= b.pMax.y);
    bool z = (pMax.z >= b.pMin.z) && (pMin.z <= b.pMax.z);
    return (x && y && z);
  }

  bool Inside(const Point &pt) const {
    return (pt.x >= pMin.x && pt.x <= pMax.x &&
            pt.y >= pMin.y && pt.y <= pMax.y &&
            pt.z >= pMin.z && pt.z <= pMax.z);
  }

  void Expand(float delta) {
    pMin -= Vector(delta, delta, delta);
    pMax += Vector(delta, delta, delta);
  }

  float SurfaceArea() const {
    Vector d = pMax - pMin;
    return 2.f * (d.x * d.y + d.x * d.z + d.y * d.z);
  }

  float Volume() const {
    Vector d = pMax - pMin;
    return d.x * d.y * d.z;
  }

  // which axes is longest: 0- x, 1- y, 2- z
  int MaximumExtent() const {
    Vector diag = pMax - pMin;
    if (diag.x > diag.y && diag.x > diag.z)
      return 0;
    else if (diag.y > diag.z)
      return 1;
    else
      return 2;
  }

  const Point &operator[](int i) const;
  Point &operator[](int i);

  Point Lerp(float tx, float ty, float tz) const {
    return Point(::Lerp(tx, pMin.x, pMax.x),
                 ::Lerp(ty, pMin.y, pMax.y),
                 ::Lerp(tz, pMin.z, pMax.z));
  }

  Vector Offset(const Point &p) const {
    return Vector((p.x - pMin.x) / (pMax.x - pMin.x),
                  (p.y - pMin.y) / (pMax.y - pMin.y),
                  (p.z - pMin.z) / (pMax.z - pMin.z));
  }

  void BoundingSphere(Point *c, float *rad) const;
  
};

#endif // __GEOMETRY__
