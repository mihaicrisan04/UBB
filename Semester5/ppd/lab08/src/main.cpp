#include "dsm.hpp"
#include <iostream>
#include <vector>
#include <sstream>
#include <thread>
#include <chrono>

struct ChangeRecord {
    int var_id;
    int old_value;
    int new_value;
};

std::vector<ChangeRecord> change_log;

void print_usage() {
    std::cout << "Usage: mpirun -np <num_processes> ./bin/lab08 [options]\n"
              << "Options:\n"
              << "  --vars N     Number of variables (default: 3)\n"
              << "  --demo       Run demonstration\n"
              << "  --test       Run total ordering test\n";
}

void run_demo(int rank, int size, int num_vars) {
    dsm::DSM mem(MPI_COMM_WORLD, num_vars);

    for (int v = 0; v < num_vars; v++) {
        std::set<int> subscribers;
        for (int p = 0; p < size; p++) {
            subscribers.insert(p);
        }
        mem.subscribe(v, subscribers);
    }

    mem.set_callback([rank](int var_id, int old_val, int new_val) {
        change_log.push_back({var_id, old_val, new_val});
        std::cout << "[P" << rank << "] Variable " << var_id
                  << " changed: " << old_val << " -> " << new_val << "\n";
    });

    mem.sync();

    if (rank == 0) {
        std::cout << "\n=== Demo: Write Operations ===\n";
    }
    mem.sync();

    mem.write(rank % num_vars, (rank + 1) * 10);
    mem.sync();

    if (rank == 0) {
        std::cout << "\n=== Demo: Compare and Exchange ===\n";
    }
    mem.sync();

    if (rank == 0) {
        int current = mem.read(0);
        std::cout << "[P0] Current value of var 0: " << current << "\n";

        bool success = mem.compare_exchange(0, current, 999);
        std::cout << "[P0] CAS(var=0, expected=" << current
                  << ", new=999): " << (success ? "SUCCESS" : "FAILED") << "\n";

        success = mem.compare_exchange(0, 0, 888);
        std::cout << "[P0] CAS(var=0, expected=0, new=888): "
                  << (success ? "SUCCESS" : "FAILED") << "\n";
    }

    mem.sync();

    if (rank == 0) {
        std::cout << "\n=== Final Values ===\n";
        for (int v = 0; v < num_vars; v++) {
            std::cout << "Variable " << v << " = " << mem.read(v) << "\n";
        }
    }

    mem.close();
}

void run_ordering_test(int rank, int size, int num_vars) {
    dsm::DSM mem(MPI_COMM_WORLD, num_vars);

    std::set<int> all_procs;
    for (int p = 0; p < size; p++) {
        all_procs.insert(p);
    }

    for (int v = 0; v < num_vars; v++) {
        mem.subscribe(v, all_procs);
    }

    change_log.clear();

    mem.set_callback([](int var_id, int old_val, int new_val) {
        change_log.push_back({var_id, old_val, new_val});
    });

    mem.sync();

    for (int round = 0; round < 3; round++) {
        for (int v = 0; v < num_vars; v++) {
            if ((v + round) % size == rank) {
                mem.write(v, rank * 100 + round * 10 + v);
            }
        }
        mem.sync();
    }

    mem.sync();

    std::ostringstream oss;
    for (const auto& rec : change_log) {
        oss << rec.var_id << ":" << rec.old_value << "->" << rec.new_value << ";";
    }
    std::string my_log = oss.str();

    if (rank == 0) {
        std::cout << "\n=== Total Ordering Test ===\n";
        std::cout << "[P0] Change sequence: " << my_log << "\n";

        bool all_match = true;
        for (int p = 1; p < size; p++) {
            int len;
            MPI_Recv(&len, 1, MPI_INT, p, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

            std::vector<char> buf(len + 1);
            MPI_Recv(buf.data(), len, MPI_CHAR, p, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            buf[len] = '\0';

            std::string other_log(buf.data());
            std::cout << "[P" << p << "] Change sequence: " << other_log << "\n";

            if (other_log != my_log) {
                std::cout << "MISMATCH between P0 and P" << p << "!\n";
                all_match = false;
            }
        }

        if (all_match) {
            std::cout << "\nSUCCESS: All processes observed the same change order!\n";
        } else {
            std::cout << "\nFAILURE: Processes observed different change orders!\n";
        }
    } else {
        int len = static_cast<int>(my_log.size());
        MPI_Send(&len, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
        MPI_Send(my_log.c_str(), len, MPI_CHAR, 0, 1, MPI_COMM_WORLD);
    }

    mem.close();
}

int main(int argc, char* argv[]) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int num_vars = 3;
    bool demo_mode = true;
    bool test_mode = false;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            if (rank == 0) print_usage();
            MPI_Finalize();
            return 0;
        } else if (arg == "--vars" && i + 1 < argc) {
            num_vars = std::stoi(argv[++i]);
        } else if (arg == "--demo") {
            demo_mode = true;
            test_mode = false;
        } else if (arg == "--test") {
            test_mode = true;
            demo_mode = false;
        }
    }

    if (rank == 0) {
        std::cout << "Distributed Shared Memory Demo\n";
        std::cout << "Processes: " << size << ", Variables: " << num_vars << "\n";
    }

    if (test_mode) {
        run_ordering_test(rank, size, num_vars);
    } else {
        run_demo(rank, size, num_vars);
    }

    MPI_Finalize();
    return 0;
}
