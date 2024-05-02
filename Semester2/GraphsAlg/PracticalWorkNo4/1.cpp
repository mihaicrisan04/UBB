#include <iostream>
#include <fstream>
#include <vector>
#include <stack>
#include <queue>
#include <cctype>
#include <string>

using namespace std;


bool isNumber(const std::string& str) {
    for (char const &c : str) {
        if (std::isdigit(c) == 0) return false;
    }
    return true;
}

bool isCyclicUtil(vector<vector<pair<int, int>>> &g, int node, vector<bool> &visited, vector<bool> &recStack) {
    visited[node] = true;
    recStack[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh.first] && isCyclicUtil(g, neigh.first, visited, recStack)) {
            return true;
        } 
        else if (recStack[neigh.first]) {
            return true;
        }
    }

    recStack[node] = false;
    return false;
}

bool isDAG(vector<vector<pair<int,int>>> &g, int n) {
    vector<bool> visited(n+1, false);
    vector<bool> recStack(n+1, false);

    for (int i = 1; i <= n; i++) {
        if (!visited[i]) {
            if (isCyclicUtil(g, i, visited, recStack)) {
                return false;
            }
        }
    } 
    
    return true;
}    

void dfs(vector<vector<pair<int, int>>> &g, int node, vector<bool> &visited, stack<int> &s) {
    visited[node] = true;

    for (auto &neigh: g[node]) {
        if (!visited[neigh.first]) {
            dfs(g, neigh.first, visited, s);
        }
    }

    s.push(node);
}

vector<int> topologicalSort(vector<vector<pair<int, int>>> &g, int n) {
    vector<bool> visited(n+1, false);
    stack<int> s; 

    for (int i = n; i >= 1; i--) {
        if (!visited[i]) {
            dfs(g, i, visited, s);
        }
    }

    vector<int> topOrder;
    while (!s.empty()) {
        topOrder.push_back(s.top());
        s.pop();
    }

    return topOrder;
}

void schedule(vector<vector<pair<int, int>>> &g, int n, vector<int> &costs, vector<vector<pair<int, int>>> &gt) {
    vector<int> inDegree(n+1, 0), earliest(n+1, 0), latest(n+1);
    vector<bool> visited(n+1, false);
    queue<int> q;
    int total = 0;

    // compute the number of dependents each activity has
    for (int i = 1; i <= n; i++) {
        for (auto &neigh: g[i]) {
            inDegree[neigh.first]++;
        }
    }

    // start from the activities that dont depend on anyone
    for (int i = 1; i <= n; i++) {
        if (inDegree[i] == 0)  {
            q.push(i);
        }
    }

    // compute the earliest starting times
    while (!q.empty()) {
        int curr = q.front();
        q.pop();

        for (auto &neigh: g[curr]) {
            if (--inDegree[neigh.first] == 0) {
                q.push(neigh.first);
            }
            earliest[neigh.first] = max(earliest[neigh.first], earliest[curr] + neigh.second);
        }
    }

    int maxEarliest = 0;
    for (int i = 1; i <= n; i++) {
        maxEarliest = max(maxEarliest, earliest[i]);
    } 

    // compute toatl time for activities
    for (int i = 1; i <= n; i++) {
        if (earliest[i] == maxEarliest) {
            total = max(total, maxEarliest + costs[i]);
        }
    }

    // reset vectors for finding the latest starting times for activities
    for (int i = 1; i <= n; i++) {
        latest[i] = maxEarliest;
        inDegree[i] = 0;
    }

    // compute the number of dependents in the transpose graph
    for (int i = 1; i <= n; i++) {
        for (auto &neigh: gt[i]) {
            inDegree[neigh.first]++;
        }
    }

    // start from the last activities
    for (int i = 1; i <= n; i++) {
        if (inDegree[i] == 0) {
            q.push(i);
        }
    }

    // compute the latest starting times 
    while (!q.empty()) {
        int curr = q.front();
        q.pop();

        for (auto &neigh: gt[curr]) {
            if (--inDegree[neigh.first] == 0) {
                q.push(neigh.first);
            }
            latest[neigh.first] = min(latest[neigh.first], latest[curr] - neigh.second);
        }
    }

    for (int i = 1; i <= n; i++) {
        cout << (char)(i + 'A' - 1) << ": earliest: " << earliest[i] << " - latest: " << latest[i] << " - duration: " << costs[i] << '\n';
    }
    cout << "Total project time: " << total << '\n';

    
    // write the critical activities
    cout << "Critical activities:\n";
    for (int i = 1; i <= n; i++) {
        if (earliest[i] == latest[i]) {
            cout << (char)(i + 'A' - 1) << '\n';
        }
    }
}

int main() {
    ifstream fin("input-1.txt");

    vector<vector<pair<int, int>>> g(30), gt(30);
    vector<int> costs(30);
    vector<vector<int>> srcs(30);
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
            srcs[dest].push_back(c[0] - 'A' + 1);
        }
    }        
    for (int i = 1; i <= n; i++) {
        for (int s: srcs[i]) {
            g[s].push_back({i, costs[s]});
            gt[i].push_back({s, costs[s]});
        }
    }

    if (isDAG(g, n)) {
        cout << "Graph is not a DAG\n";
        return 0;
    }

    cout << "Topological Sort of the activities:\n";
    vector<int> topOrder = topologicalSort(g, n);
    for (auto &i: topOrder) {
        cout << (char)(i + 'A' - 1) << '\n';
    }
    cout << '\n';

    // earliest and latest time for each activitiy + total time of the project + critical activities
    schedule(g, n, costs, gt);

    fin.close();
    return 0;
}