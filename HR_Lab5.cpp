#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

double median(vector<int>);

int main()
{
    int n, in;
    cin >> n;
    int a[n];

    for (int i = 0; i < n; i++) {
        cin >> in;
        a[i] = in;
    }

    vector<int> v(a, a + n);

    cout << median(v) << "\n";
}

double median(vector<int> v) {
    sort(v.begin(), v.end());
    if (v.size() % 2 == 0) {
        return (double)(v[v.size() / 2 - 1] + v[v.size() / 2]) / 2;
    }
    else {
        return (double)v[v.size() / 2];
    }
}

