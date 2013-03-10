#include <iostream>
#include <cstring>

using namespace std;

char board[100][100];

int main(int argc, char *argv[]) {
  int field = 1;
  int n, m;
  cin >> n >> m;
  while (1) {
    if (!(n && m)) break;
    memset(board, 0, 100 * 100);
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        cin >> board[i][j];
      }
    }
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        if (board[i][j] == '*') continue;
        int num = 0;
        if (j > 0 && i > 0)
          num += (board[i - 1][j - 1] == '*');
        if (j > 0)
          num += (board[i][j - 1] == '*');
        if (j > 0 && i < n-1)
          num += (board[i + 1][j - 1] == '*');
        if (i > 0)
          num += (board[i - 1][j] == '*');
        if (i < n-1)
          num += (board[i + 1][j] == '*');
        if (i > 0 && j < m-1)
          num += (board[i - 1][j + 1] == '*');
        if (j < m-1)
          num += (board[i][j + 1] == '*');
        if (i < n-1 && j < m-1)
          num += (board[i + 1][j + 1] == '*');
        board[i][j] = '0' + num;
      }
    }
    cout << "Field #" << field++ << ":" << endl;
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        cout << board[i][j];
      }
      cout << endl;
    }
    cin >> n >> m;
    if (n && m) cout << endl;
  }
  return 0;
}
