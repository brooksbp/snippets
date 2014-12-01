#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

void dfs_go(const vector<int>& board, vector<int>& path, int path_len, int& min_path_len) {
  if (path_len > min_path_len) {
    // Short circuit this dfs path if already found a shorter one..
    return;
  }

  int here = path[path_len];

  if (here == 100) {
    if (path_len < min_path_len) {
      min_path_len = path_len;
    }
    return;
  }

  for (int i = here+1; (i <= here+6) && (i <= 100); i++) {
    // Follow each unfollowed ladder or snake.
    if (board[i] != 0) {
      auto it = find(path.begin(), path.end(), i);
      if (it == path.end()) {
        path[path_len+1] = i;
        path[path_len+2] = board[i];
        dfs_go(board, path, path_len+2, min_path_len);
      }
    }
  }
  // Also try advancing to the next furthest spot that is not a ladder or snake.
  for (int i = min(100, here+6); i >= here+1; i--) {
    if (board[i] == 0) {
      path[path_len+1] = i;
      dfs_go(board, path, path_len+1, min_path_len);
      break;
    }
  }
}

int main() {
  int T; cin >> T;
  while (T--) {
    int num_ladders; cin >> num_ladders;
    char c; cin >> c;
    int num_snakes; cin >> num_snakes;

    vector<int> board(101, 0);

    int n = num_ladders + num_snakes;
    while (n--) {
      int s; cin >> s;
      char c; cin >> c;
      int e; cin >> e;
      board[s] = e;
    }

    vector<int> path(100, 0);
    path[0] = 1;
    int min_path_len = 101;
    dfs_go(board, path, 0, min_path_len);

    cout << min_path_len-1 << endl;
  }
  return 0;
}
