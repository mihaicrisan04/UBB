#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;


int main() {
    ifstream fin ("bridge-torch.in");
    int n, x;
    vector<int> times;

    fin >> n;
    times = vector<int>(n+1);
    for (int i = 1; i <= n; i++) {
        fin >> times[i];
    }

    sort(times.begin() + 1, times.end());

    //...
    
    fin.close();
    return 0;
}