#include <iostream>

using namespace std;

void print_array (int *A, int len) {
    for (int i = 0; i < len; i++) {
        cout << A[i] << " ";
    }
    cout << endl;
}

// CLRS is 1-based...
void insertion_sort (int *A, int len) {
    for (int j = 1; j < len; j++) {
        int key = A[j];
        int i = j - 1;
        while (i >= 0 && A[i] > key) {
            A[i + 1] = A[i];
            i -= 1;
        }
        A[i + 1] = key;
    }
}

int main (void) {
    int array[10] = {
        5,2,8,4,1,7,6,3,9,10
    };    
    print_array(array, 10);
    insertion_sort(array, 10);
    print_array(array, 10);
    return 0;
}
