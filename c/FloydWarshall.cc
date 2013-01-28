#include <vector>
#include <utility>
#include <iostream>

using namespace std;

struct Vertex {
  Vertex() : id(0) { }
  Vertex(int id_) : id(id_) { }
  void add_edge(int weight, Vertex* vert) {
    edges.push_back(make_pair(weight, vert));
  }
  vector<pair<int, Vertex*> > edges;  
  int id;
};

int main(int argc, char *argv[]) {
  Vertex v1(1);
  Vertex v2(2);
  Vertex v3(3);
  Vertex v4(4);
  v1.add_edge(-2, &v3);
  v2.add_edge(4, &v1);
  v2.add_edge(3, &v3);
  v3.add_edge(2, &v4);
  v4.add_edge(-1, &v2);
  vector<Vertex*> graph;
  graph.push_back(&v1);
  graph.push_back(&v2);
  graph.push_back(&v3);
  graph.push_back(&v4);

  int dist[4][4] = {
    9, 9, 9, 9,
    9, 9, 9, 9,
    9, 9, 9, 9,
    9, 9, 9, 9
  };

  for (auto it = graph.begin(); it != graph.end(); it++) {
    dist[(*it)->id -1][(*it)->id -1] = 0;
  }
  
  for (auto it = graph.begin(); it != graph.end(); it++) {
    for (auto vit = (*it)->edges.begin(); vit != (*it)->edges.end(); vit++) {
      dist[(*it)->id -1][vit->second->id -1] = vit->first;
    }
  }

  for (int i = 0; i < 4; i++) {
    cout << dist[i][0] << dist[i][1] << dist[i][2] << dist[i][3] << endl;
  }

  for (int k = 0; k < 4; k++) {
    for (int i = 0; i < 4; i++) {
      for (int j = 0; j < 4; j++) {
        if (dist[i][k] + dist[k][j] < dist[i][j]) {
          dist[i][j] = dist[i][k] + dist[k][j];
        }
      }
    }
  }

  for (int i = 0; i < 4; i++) {
    cout << dist[i][0] << dist[i][1] << dist[i][2] << dist[i][3] << endl;
  }

  return 0;
}
