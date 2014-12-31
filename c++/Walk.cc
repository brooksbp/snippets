#include <iostream>
#include <vector>
#include <unordered_map>
#include <utility>
#include <gperftools/profiler.h>

enum { U, D, L, R, };
enum { WHITE, RED };

typedef std::pair<int,int> Coord;

struct pairhash {
 public:
  template <typename T, typename U>
  std::size_t operator()(const std::pair<T, U> &x) const {
    return std::hash<T>()(x.first) ^ std::hash<U>()(x.second);
  }
};

typedef std::unordered_map<Coord, int, pairhash> Board;

int get_color(Board& b, Coord& c) {
  auto it = b.find(c);
  if (it == b.end()) {
    b[c] = WHITE;
  }
  return b[c];
}

struct Knight {
  Coord location; // (x, y) <-> (location.first, location.second)
  int direction;
};

void rotate(Knight& k, int direction) {
  int new_direction = k.direction;
  switch (direction) {
    case R:
      switch (k.direction) {
        case U: new_direction = R; break;
        case D: new_direction = L; break;
        case L: new_direction = U; break;
        case R: new_direction = D; break;
      }
      break;
    case L:
      switch (k.direction) {
        case U: new_direction = L; break;
        case D: new_direction = R; break;
        case L: new_direction = D; break;
        case R: new_direction = U; break;
      }
      break;
    default:
      std::cout << "not a 90 degree rotation!\n";
  }
  k.direction = new_direction;
}

void move_forward(Knight& k) {
  switch (k.direction) {
    case U: k.location.second++; break;
    case D: k.location.second--; break;
    case L: k.location.first--; break;
    case R: k.location.first++; break;
  }
}

void step(Board& b, Knight& k, std::vector<int>& p) {
  // Get color of current position.
  int color = get_color(b, k.location);

  // Rotate knight.
  rotate(k, (color == WHITE) ? R : L);

  // Flip color of current position.
  b[k.location] = (color == WHITE) ? RED : WHITE;

  // Move +1 in new direction and record to path.
  move_forward(k);
  p.push_back(k.direction);
}

// Check whether V[x+0] == V[y+0], V[x+1] == V[y+1], .. V[x+k] == V[y+k].
bool is_match(std::vector<int>& V, int x, int y, int k) {
  bool match = true;
  int i = 0;
  while (k--) {
    if (V[x+i] != V[y+i]) {
      match = false;
      break;
    }
    i++;
  }
  return match;

}

#if 0
// Find longest k-length (where k >= K) subset of path that occurs twice
// non-overlapping. Returns true if such cycle exists, false otherwise.
bool FindMaxCycle(std::vector<int>& V, int& x, int& y, int& K) {

  bool found = false;

  for (size_t k = std::max(1, K+1); k <= V.size()/2; k++) {

    int a = 0;
    int b = a + k;
    int n = V.size() - 2*k + 1;
    while (n--) {
      if (is_match(V, a, b, k)) {
        x = a;
        y = b;
        K = k;
        found = true;
        // Break out of the while-loop to avoid finding any more cycles with
        // length equal to k.
        break;
      }
      a++;
      b++;
    }

  }

  return found;
}
#endif

// FindMaxCycle variant which only searches k-sized cycles ending in the
// newly-appended 'move' from the previous call to 'step'. This avoids much
// duplicated computation in the above FindMaxCycle.
bool FindMaxCycle(std::vector<int>& V, int& x, int& y, int& K) {
  bool found = false;

  for (size_t k = V.size()/2; (int)k > K; k--) {

    int b = V.size() - k;
    int a = b - k;

    if (is_match(V, a, b, k)) {
      x = a;
      y = b;
      K = k;
      found = true;
      break;
    }

  }
  return found;
}

int main() {
  Board b;

  Knight k;
  k.location.first = 0;
  k.location.second = 0;
  k.direction = U;

  std::vector<int> path;

  int x, y;
  int MaxCycleLen = 0;

  int n = 1000000;
  while (n--) {
    step(b, k, path);

    if (FindMaxCycle(path, x, y, MaxCycleLen) || (n % 1000 == 0)) {
      std::cout << "Steps=" << path.size()
                << " LargestCycleLen=" << MaxCycleLen
                << " mod-104=" << (MaxCycleLen % 104 == 0)
                << " x=" << x
                << " y=" << y
                << std::endl;
      ProfilerFlush();
    }
  }

  return 0;
}
