#include <iostream>
#include <vector>
#include <algorithm>

#include "CenteredIntervalTree.h"

using namespace std;

int solve() {
  int W, H, P, Q, N, X, Y, a, b, c, d;
  cin >> W >> H >> P >> Q >> N >> X >> Y >> a >> b >> c >> d;

  vector<int> x, y;
  x.push_back(X);
  y.push_back(Y);
  for (int i = 1; i < N; i++) {
    int xx = ((x.back() * a) + (y.back() * b) + 1) % W;
    int yy = ((x.back() * c) + (y.back() * d) + 1) % H;
    x.push_back(xx);
    y.push_back(yy);
  }

  vector<int> D[40000];
  for (int i = 0; i < N; i++) D[y[i]].push_back(x[i]);
  for (int i = 0; i < H; i++) {
    sort(D[i].begin(), D[i].end());
    D[i].erase(unique(D[i].begin(), D[i].end()), D[i].end());
  }

  cout << "here1\n";
  int ans = 0;
  for (int y = 0; y < (H - Q + 1); y++) {
    std::vector<std::pair<int,int> > dp;
    for (int yy = 0; yy < Q; yy++) {
      for (int x = 0; x < D[y+yy].size(); x++) {
        dp.push_back({D[y+yy][x],D[y+yy][x]});
      }
    }
    CenteredIntervalTree t;
    t.construct(dp);
    for (int x = 0; x < (W - P + 1); x++) {
      if (!t.query_interval(x, x+P-1)) ans += 1;
    }    
  }
  cout << "here2\n";
  return ans;
}

int main(int argc, char *argv[]) {
  int n; cin >> n;
  for (int t = 0; t < n; t++) {
    std::cout << "Case #" << (t+1) << ": " << solve() << std::endl;
  }
  return 0;
}

// int main(int argc, char *argv[]) {

//   CenteredIntervalTree t;

//   std::vector<std::pair<int,int> > intervals;
//   intervals.push_back({0,1});
//   intervals.push_back({3,7});
//   intervals.push_back({6,8});
//   intervals.push_back({7,8});
//   intervals.push_back({12,15});
//   intervals.push_back({13,14});
//   intervals.push_back({20,50});

//   t.construct(intervals);

//   for (int i = 0; i < 8; i++) {
//     std::cout << i << " " << t.query_point(i) << std::endl;
//   }

//   std::cout << "t.query_interval(0,1) " << t.query_interval(0,1) << std::endl;
//   std::cout << "t.query_interval(9,11) " << t.query_interval(9,11) << std::endl;
//   std::cout << "t.query_interval(9,60) " << t.query_interval(9,60) << std::endl;
  
//   return 0;
// }
