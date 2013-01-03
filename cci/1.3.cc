// Given two strings, write a method to decide if one is a
// permutation of the other.

#include <iostream>
#include <string>
#include <cassert>
#include <unordered_map>

using namespace std;

bool is_permutation(string& s1, string& s2) {
  unordered_map<char, int> s1_map, s2_map;
  unordered_map<char, int>::const_iterator i, j;
  string::iterator it;

  if (s1.length() != s2.length()) return false;

  for (it = s1.begin(); it < s1.end(); it++) s1_map[*it] += 1;
  for (it = s2.begin(); it < s2.end(); it++) s2_map[*it] += 1;

  if (s1_map.size() != s2_map.size()) return false;

  for (i = s1_map.cbegin(); i != s1_map.cend(); i++) {
    j = s2_map.find(i->first);
    if (j == s2_map.cend()) return false;
    else if (i->second != j->second) return false;
  }
  return true;
}

int main(int argc, char *argv[]) {
  string s1("permuu");
  string s2("umurep");
  string s3("notpermu");
  string s4("notper");
  string s5("");  
  assert(is_permutation(s1, s2));
  assert(!is_permutation(s1, s3));
  assert(!is_permutation(s1, s4));
  assert(!is_permutation(s1, s5));
  assert(is_permutation(s5, s5));
  return 0;
}
