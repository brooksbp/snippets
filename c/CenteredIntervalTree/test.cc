#include "CenteredIntervalTree.h"

int main(int argc, char *argv[]) {

  CenteredIntervalTree t;

  std::vector<std::pair<int,int> > intervals;
  intervals.push_back({0,1});
  intervals.push_back({3,7});
  intervals.push_back({6,8});
  intervals.push_back({7,8});


  t.construct(intervals);
  t.printCenter();
  
  return 0;
}
