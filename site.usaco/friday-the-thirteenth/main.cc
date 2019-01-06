/*
ID: brooks.1
PROG: friday
LANG: C++11
*/
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

bool is_leap_year(int year) {
  if (year % 100 == 0) {
    return (year % 400 == 0) ? true : false;
  }
  return (year % 4 == 0) ? true : false;
}
int days_in_year(int year) {
  return is_leap_year(year) ? 366 : 365;
}
int days_in_month(int month, bool leap) {
  switch (month) {
    case 1: return 31;
    case 2: return (leap) ? 29 : 28;
    case 3: return 31;
    case 4: return 30;
    case 5: return 31;
    case 6: return 30;
    case 7: return 31;
    case 8: return 31;
    case 9: return 30;
    case 10: return 31;
    case 11: return 30;
    case 12: return 31;
  }
  return -1;
}
int days_in_week(void) {
  return 7;
}

int main() {
  ifstream fin("friday.in");
  ofstream fout("friday.out");

  int N; fin>>N;

  int f[7] = {0};

  int year = 1900;
  int month = 1;
  int day = 1;
  int dow = 1;

  for (int y = year; y < year+N; y++) {
    for (int m = month; m <= 12; m++) {
      for (int d = day; d <= days_in_month(m, is_leap_year(y)); d++) {
        if (d == 13) f[dow-1]++;
        if (++dow > 7) dow = 1;
      }
    }
  }

  fout << f[5] << " ";
  fout << f[6] << " ";
  fout << f[0] << " ";
  fout << f[1] << " ";
  fout << f[2] << " ";
  fout << f[3] << " ";
  fout << f[4];
  fout << endl;

  return 0;
}
