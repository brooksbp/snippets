#include <vector>
#include <algorithm>

using namespace std;

// Monotone chain convex hull algorithm: first sort points
// by x-coordinate (in case of tie, sort by y-coordinate),
// then construct upper and lower hulls.

struct Point {
  unsigned int x, y;

  bool operator<(const Point& p) const {
    return x < p.x || (x == p.x && y < p.y);
  }
};

vector<Point> convex_hull(vector<Point> P) {
  vector<Point> ch;

  sort(P.begin(), P.end());

  vector<Point> upper, lower;

  for (int i = 0; i < P.size(); i++) {
    while (lower.size() > 1 && /* last 2 pts + P[i] does not make a CCW turn */) {
      lower.pop_back();
    }
    lower.push_back(P[i]);
  }

  // .........................
  
  return ch;
}

int main(int argc, char *argv[]) {
  
  return 0;
}
