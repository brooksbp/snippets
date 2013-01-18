// Write a function that takes an arithmetical expression in RPN
// and returns the number that the expression evaluates to.
// "3,4,*,1,2,+,+" -> "12,3,+" -> 15
// A,B,? where A and B are RPN expr and ? is an operation.

#include <iostream>
#include <string>
#include <stack>

using namespace std;

// My mistake was an approach with R->L.. harder than L->R

int eval_rpn(string expr) {
  stack<int> operands;
  
  for (string::iterator it = expr.begin(); it != expr.end(); ++it) {
    if (*it == ',') {
      continue;
    }
    if (*it == '+' || (*it == '-' && *(it+1) == ',') || *it == '*' || *it == '/') {
      if (2 > operands.size()) {
        throw "parse error";
      }
      int a = operands.top(); operands.pop();
      int b = operands.top(); operands.pop();
      int c;
      switch (*it) {
        case '+': c = b + a; break;
        case '-': c = b - a; break;
        case '*': c = b * a; break;
        case '/': c = b / a; break;
      }
      operands.push(c);
    } else {
      int x = 0;
      bool negative = false;
      if (*it == '-') {
        negative = true;
        it++;
      }      
      while (it != expr.end() && *it != ',') {
        x = x * 10 + (*it - '0');
        it++;
      }
      if (negative) {
        x = -x;
      }
      operands.push(x);
    }
  }
  return operands.top();
}

int main(int argc, char *argv[]) {
  cout << eval_rpn(string("1,3,+")) << endl;
  cout << eval_rpn(string("10,30,+")) << endl;
  cout << eval_rpn(string("1,3,5,-,+")) << endl;
  cout << eval_rpn(string("-11,6,+")) << endl;
  return 0;
}
