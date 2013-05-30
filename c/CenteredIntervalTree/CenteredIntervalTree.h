#ifndef CENTERED_INTERVAL_TREE_H_
#define CENTERED_INTERVAL_TREE_H_

#include <algorithm>
#include <iostream>
#include <memory>
#include <vector>
#include <utility>

struct sort_second {
  bool operator()(const std::pair<int,int> &lhs,
                  const std::pair<int,int> &rhs) {
    return lhs.second < rhs.second;
  }
};

struct CenteredIntervalTree {
 public:
  struct node {
    int x_center;
    std::vector<std::pair<int,int> > start, end;
    std::shared_ptr<struct node> left, right;
  };
  std::shared_ptr<struct node> root;

  CenteredIntervalTree() {
    root = std::make_shared<struct node>();
  }
  ~CenteredIntervalTree() { }
  
  void construct_(std::vector<std::pair<int,int> > intervals,
                  std::shared_ptr<struct node> node) {
    if (intervals.size() < 1) return;

    std::sort(intervals.begin(), intervals.end(), sort_second());
    int rightmost = intervals.back().second;    
    std::sort(intervals.begin(), intervals.end());
    int leftmost = intervals.front().first;    
    node->x_center = ((rightmost - leftmost) / 2) + leftmost;

    std::vector<std::pair<int,int> > S_left, S_center, S_right;
    for (auto it = intervals.begin(); it != intervals.end(); it++) {
      if (it->first < node->x_center && it->second < node->x_center) {
        S_left.push_back(*it);
      } else if (it->first <= node->x_center && it->second <= node->x_center) {
        S_center.push_back(*it);
      } else if (it->first > node->x_center) {
        S_right.push_back(*it);
      }
    }
    if (S_center.size() > 0) {
      std::sort(S_center.begin(), S_center.end());
      node->start = S_center;
      std::sort(S_center.begin(), S_center.end(), sort_second());
      node->end = S_center;
    }
    if (S_left.size() > 0) {
      node->left = std::make_shared<struct node>();
      construct_(S_left, node->left);
    }
    if (S_right.size() > 0) {
      node->right = std::make_shared<struct node>();
      construct_(S_right, node->right);
    }
  }
  void construct(std::vector<std::pair<int,int> > intervals) {
    construct_(intervals, root);
  }

  void printCenter() {
    std::cout << "center: " << root->x_center << std::endl;
  }
};

#endif // CENTERED_INTERVAL_TREE_H_
