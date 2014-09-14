#include <iostream>
#include <vector>
#include <string>
#include <utility>
#include <unordered_map>

using namespace std;

enum {
  UNVISITED,
  VISITED
};

enum {
  LEFT,
  RIGHT,
  UP,
  DOWN
};

string move2str(int move) {
  switch(move) {
    case LEFT:  return string("LEFT");
    case RIGHT: return string("RIGHT");
    case UP:    return string("UP");
    default:    return string("DOWN");
  }
}

// K <x,y>, V <visited,first_move>
typedef unordered_map<pair<int,int>, pair<int, int> > node_map_t;

// Specialize std::hash<std::pair<int, int> > so we can hash it
namespace std {
template <typename T, typename U>
struct hash<pair<T, U> > {
 public:
  size_t operator()(pair<T, U> x) const throw() {
    return hash<T>()(x.first) ^ hash<U>()(x.second);
  }
};
}

void push_possible_moves(int x, int y, int first_dir, node_map_t &node_map) {
  pair<int, int> K;
  pair<int, int> V = make_pair(UNVISITED, first_dir);
  
  if (y != 0) {
    K = make_pair(x, y -1);
    if (!node_map.count(K)) node_map[K] = V;
  }
  if (y != 4) {
    K = make_pair(x, y +1);
    if (!node_map.count(K)) node_map[K] = V;
  }
  if (x != 0) {
    K = make_pair(x -1, y);
    if (!node_map.count(K)) node_map[K] = V;
  }
  if (x != 4) {
    K = make_pair(x +1, y);
    if (!node_map.count(K)) node_map[K] = V;
  }  
}

void __next_move(vector<string> board, node_map_t &node_map) {
  for (auto it = node_map.begin(); it != node_map.end(); it++) {
    if (it->second.first == VISITED) {
      continue;
    }
    if (board[it->first.first][it->first.second] == 'd') {
      cout << move2str(it->second.second) << endl; exit(0);
    }
    it->second.first = VISITED;
    push_possible_moves(it->first.first, it->first.second,
                        it->second.second, node_map);
  }
  __next_move(board, node_map);
}

void next_move(int posx, int posy, vector<string> board) {
  if (board[posx][posy] == 'd') {
    cout << "CLEAN" << endl;
    return;
  }  

  node_map_t node_map;
  node_map[make_pair(posx, posy)] = make_pair(VISITED, 0);

  if (posy != 0) {
    node_map[make_pair(posx, posy -1)] = make_pair(UNVISITED, LEFT);
  }
  if (posy != 4) {
    node_map[make_pair(posx, posy +1)] = make_pair(UNVISITED, RIGHT);
  }
  if (posx != 0) {
    node_map[make_pair(posx -1, posy)] = make_pair(UNVISITED, UP);
  }
  if (posx != 4) {
    node_map[make_pair(posx +1, posy)] = make_pair(UNVISITED, DOWN);
  }
  __next_move(board, node_map);
}

int main(int argc, char *argv[]) {
  int pos[2];
  vector<string> board;
  cin >> pos[0] >> pos[1];
  for (int i = 0; i < 5; i++) {
    string s; cin >> s;
    board.push_back(s);
  }
  next_move(pos[0], pos[1], board);
  return 0;
}
