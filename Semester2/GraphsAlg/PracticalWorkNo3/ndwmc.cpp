#include <iostream>
#include <fstream>
#include <vector>
#include <queue>

using namespace std;

const int INF = 1e9;

ifstream fin ("ndwmc.in");

void dijkstra(vector<vector<pair<int,int>>>& g, int n, int a, int b) {

}

int main() {
    int n, m, a, b;
    vector<vector<pair<int,int>>> g;

    fin >> n >> m >> a >> b;
    g = vector<vector<pair<int, int>>>(n+1);

    for (int i = 0; i < m; i++) {
        int x, y, c;
        fin >> x >> y >> c;
        g[x].push_back({y, c});
    }

    fin.close();
    return 0;
}