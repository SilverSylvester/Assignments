#include <iostream>

using namespace std;

double birthday(int);

int main() {
    int n;
    cin >> n;
    printf("%.3f\n", 1 - birthday(n));
}

double birthday(int n) {
    if (n < 2) return 1;
    else return (366 - (double)n) / 365 * birthday(n - 1);
}

