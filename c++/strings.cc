#include <iostream>
#include <string>

using namespace std;

// foldl' sum (filter isNumeric xs)
void p1() {
  string s = "96h11k4959q615948s50922o38h1453ij38w73413d5577lzrqw3780b389750vf100zd29z73j5wh73l6965n85vm77cw10awrjr29265289222238n10013uk10062f9449acbhfgcm35j78q80";
  int acc = 0;

  // for (size_t i = 0; i < s.size(); i++) {
  //   if (s[i] >= '0' && s[i] <= '9') {
  //     acc += s[i] - '0';
  //   }
  // }

  for (auto c : s) {
    if (c >= '0' && c <= '9')
      acc += c - '0';
  }

  cout << acc << endl;
}

// Naive string search.
void p2() {
  string s = "AUDRYBERNITAEATVIVANBROOKSEATJENIEATMICHALELASANDRALATIAEATLIDIAEATDEANDRAEATSYBILEATMONROEEATLATRISHAALTAEATDERICKEATROSANNLEVILIBBYKIRSTENCHARLESEATELLYNEATJANEEEATSTASIAEATJULIETTARANDIEATNORBERTSAGEEATARACELIKATINAMERNAEATISAIASWINNIEEATARLETHAEATMILOCAMIEEATANNABELEATLEANORABERTHASYBLECHANAEATREAGANERICKVALENTINAEATDORETHEASEBASTIANKRISROBERTAEATIVONNEEATAI";
  string t = "EAT";
  int count = 0;

  for (size_t i = 0; i < (s.size() - t.size() + 1); i++) {
    bool found = true;
    for (size_t j = 0; j < t.size(); j++) {
      if (s[i+j] != t[j]) found = false;
    }
    if (found) count++;
  }

  cout << count << endl;
}

int main() {
  p1();
  p2();
  return 0;
}
