#include <iostream>
#include <cstdio>
#include <cmath>
#include <vector>

using namespace std;

double dollar2dbl(string& s) {
  bool decimal = false;
  double acc = 0.0;
  for (int i = 0; i < s.length(); i++) {
    if (s[i] == '.') {
      decimal = true;
      continue;
    }
    int n = s[i] - '0';
    if (!decimal) {
      acc = (acc * 10.0) + n;
    } else {
      if (i == (s.length() - 1)) {
        acc += n / 100.0;
      } else {
        acc += n / 10.0;
      }
    }
  }
  return acc;
}

int main(int argc, char *argv[]) {
  int n;
  while (cin >> n) {
    if (!n) break;
    string s;
    vector<double> k;
    for (int i = 0; i < n; i++) {
      cin >> s;
      k.push_back(dollar2dbl(s));
    }
    double acc = 0;
    for (int i = 0; i < k.size(); i++) {
      acc += k[i];
    }
    double avg = acc / k.size();
    avg = floorf(avg * 100) / 100;

    double xchg = 0;
    for (int i = 0; i < k.size(); i++) {
      if (k[i] < avg) {
        xchg += avg - k[i];
      }
    }

    int thousands = xchg / 1000;
    if (thousands)
      printf("$%d,%.2f\n", thousands, fmod(xchg, 1000.00));
    else
      printf("$%.2f\n", xchg);
  }
  return 0;
}
