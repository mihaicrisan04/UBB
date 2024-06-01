#include <iostream>
#include <fstream>
#include <vector>
#include <stack>

using namespace std;

typedef pair<int, int> pii;
typedef vector<int> vi;
typedef vector<vi> vvi;
typedef vector<bool> vb;
typedef vector<pii> vii;
typedef vector<vii> vvii;

#define INF 1e9

bool isNumber(const std::string& str) {
    for (char const &c : str) {
        if (std::isdigit(c) == 0) return false;
    }
    return true;
}

void dfs(int u, vvi &g, vb &visited, stack<int> &st) {
    visited[u] = true;
    for (int v : g[u]) {
        if (!visited[v]) {
            dfs(v, g, visited, st);
        }
    }
    st.push(u);
}

void Schedule(int n, vvi &gin, vvi &gout, vi &costs) {;
    stack<int> st;
    vb visited(n, false), critical(n, false);
    vi es(n + 1, 0), ef(n + 1, 0), ls(n + 1, INF), lf(n + 1, INF);
    vi topSort;
    int maxEf = 0;

    for (int i = 1; i < n; i++) {
        if (!visited[i]) {
            dfs(i, gout, visited, st);
        }
    }

    topSort.push_back(0); // dummy node
    while (!st.empty()) {
        topSort.push_back(st.top());
        st.pop();
    }

    for (int i = 1; i <= n; i++) {
        int u = topSort[i];
        for (int v : gin[u]) {
            es[u] = max(es[u], ef[v]);
        }
        ef[u] = es[u] + costs[u];
        maxEf = max(maxEf, ef[u]);
    }

    for (int i = 1; i <= n; i++) {
        if (gout[i].size() == 0) {
            lf[i] = maxEf;
        }
    }
    for (int i = n; i >= 1; i--) {
        int u = topSort[i];
        for (int v : gout[u]) {
            lf[u] = min(lf[u], ls[v]);
        }
        ls[u] = lf[u] - costs[u];
    }

    for (int i = 1; i <= n; i++) {
        if (es[i] == ls[i] && ef[i] == lf[i]) {
            critical[i] = true;
        }
    }

    for (int i = 1; i <= n; i++) {
        cout << (char)('A' + i - 1) << " " << es[i] << " " << ef[i] << " " << ls[i] << " " << lf[i] << " " << critical[i] << endl;
    }
}

int main() {
    ifstream fin("Schedule.in");

    vvi gin(30), gout(30);
    vi costs(30);
    int n = 0;
    string c;

    while (fin >> c) {
        n++;
        int dest = c[0] - 'A' + 1;
        while (fin >> c) {
            if (isNumber(c)) {
                costs[dest] = stoi(c);
                break;
            }
            int src = c[0] - 'A' + 1;
            gout[src].push_back(dest);
            gin[dest].push_back(src);
        }
    } 

    Schedule(n, gin, gout, costs);    

    fin.close();
    return 0;
}