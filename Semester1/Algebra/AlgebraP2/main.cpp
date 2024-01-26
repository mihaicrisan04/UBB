#include <iostream>
#include <vector>
#include <fstream>

using namespace std;

// Author: Mihai-Dan Crisan group 913

/**
 * Prints a table of integers with each element incremented by 1.
 * 
 * @param table A 2D vector of integers representing the table to be printed.
 * @param n An integer representing the number of rows and columns in the table.
 */
void print_table(const vector<vector<int> >& table, int n, ofstream& fout) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            fout << table[i][j] + 1 << " ";
        }
        fout << endl;
    }
    fout << string(20, '-') << endl;
}

/**
 * Prints the solutions for the given system of equations.
 * 
 * @param solutions A vector of vectors of vectors representing the solutions.
 * @param n The number of variables in the system.
 */
void print_tables(const vector<vector<vector<int> > >& solutions, int n, ofstream& fout) {
    for (int i = 0; i < solutions.size(); i++) {
        fout << "Table number: " << i + 1 << endl;
        print_table(solutions[i], n, fout);
    }
}

/**
 * Checks if a binary operation defined by a table is associative.
 * @param table The table representing the binary operation.
 * @param n The size of the table.
 * @return True if the binary operation is associative, false otherwise.
 */
bool check_associativity(const vector<vector<int> >& table, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            for (int k = 0; k < n; k++) {
                if (table[table[i][j]][k] != table[i][table[j][k]]) {
                    return false;
                }
            }
        }
    }
    return true;
}

/**
 * Computes the operation tables for a given pair of matrices i and j, of size n x n.
 * The resulting tables are stored in the current_table parameter.
 * All possible solutions are stored in the solutions parameter.
 *
 * @param current_table The current operation table being computed.
 * @param i The first matrix.
 * @param j The second matrix.
 * @param n The size of the matrices.
 * @param solutions A vector of all possible solutions.
 */
void compute_operation_tables(vector<vector<int> >& current_table, int i, int j, int n, vector<vector<vector<int> > >& solutions) {
    for (int k = 0; k < n; k++) {
        current_table[i][j] = k;
        if (i == n - 1 && j == n - 1) {
            if (check_associativity(current_table, n)) {
                solutions.push_back(current_table);
            }
        } else {
            if (j == n - 1) {
                compute_operation_tables(current_table, i + 1, 0, n, solutions);
            } else {
                compute_operation_tables(current_table, i, j + 1, n, solutions);
            }
        }
    }
}

/**
 * Generates all possible solutions for a given n x n operation table.
 * 
 * @param n The size of the operation table.
 * @return A vector of all possible solutions, where each solution is represented as a vector of vectors.
 */
vector<vector<vector<int> > > generate_solutions(int n) {
    vector<vector<vector<int> > > solutions;
    vector<vector<int> > table(n, vector<int>(n, 0));
    compute_operation_tables(table, 0, 0, n, solutions);
    return solutions;
}

int main() {
    for (int i = 1; i <= 5; i++) {
        string filename_in = "input/input" + to_string(i) + ".txt";
        string filename_out = "output/output" + to_string(i) + ".txt";

        ifstream fin(filename_in);
        ofstream fout(filename_out);

        int n;
        fin >> n;

        vector<vector<vector<int> > > solutions = generate_solutions(n);
        fout << "Number of associative operations: " << solutions.size() << endl;

        if (n <= 4) {
            print_tables(solutions, n, fout);
        }

        fin.close();
        fout.close();
    }
    return 0;
}