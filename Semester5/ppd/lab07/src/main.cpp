#include "polynomial.hpp"
#include "multiply.hpp"
#include "mpi_multiply.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <chrono>
#include <iomanip>
#include <cstring>
#include <mpi.h>

struct Config {
    std::string algorithm = "naive";
    std::string mode = "seq";
    int size = 1000;
    int repeats = 3;
    bool compare = false;
    bool verify = true;
    int karatsubaSeqBase = 64;
};

struct BenchResult {
    Polynomial result;
    double avgMs;
    double minMs;
};

Config parseArgs(int argc, char** argv);
void printHelp();

BenchResult benchmark(const std::string& label, const Config& cfg,
                      const Polynomial& a, const Polynomial& b,
                      std::function<Polynomial()> fn, int rank);

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);

    int rank, numProcs;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &numProcs);

    Config cfg = parseArgs(argc, argv);

    Polynomial A = Polynomial::allOnes(cfg.size);
    Polynomial B = Polynomial::allOnes(cfg.size);

    if (rank == 0) {
        std::cout << "Config:\n";
        std::cout << "  algorithm = " << cfg.algorithm << "\n";
        std::cout << "  mode      = " << cfg.mode << "\n";
        std::cout << "  procs     = " << numProcs << "\n";
        std::cout << "  size      = " << cfg.size << "\n";
        std::cout << "  repeats   = " << cfg.repeats << "\n";
        std::cout << "  compare   = " << (cfg.compare ? "true" : "false") << "\n";
        std::cout << "  verify    = " << (cfg.verify ? "true" : "false") << "\n";
        std::cout << "  karatsuba seqBase = " << cfg.karatsubaSeqBase << "\n\n";

        if (cfg.size <= 20) {
            std::cout << "Polynomial A: " << A.toString() << "\n";
            std::cout << "Polynomial B: " << B.toString() << "\n\n";
        }
    }

    std::vector<BenchResult> results;

    if (cfg.compare) {
        results.push_back(benchmark("naive seq", cfg, A, B, [&]() {
            return multiply::naiveSeq(A, B);
        }, rank));

        MPI_Barrier(MPI_COMM_WORLD);

        results.push_back(benchmark("naive mpi", cfg, A, B, [&]() {
            return mpi_multiply::naiveMPI(A, B, MPI_COMM_WORLD);
        }, rank));

        MPI_Barrier(MPI_COMM_WORLD);

        results.push_back(benchmark("karatsuba seq", cfg, A, B, [&]() {
            return multiply::karatsubaSeq(A, B, cfg.karatsubaSeqBase);
        }, rank));

        MPI_Barrier(MPI_COMM_WORLD);

        results.push_back(benchmark("karatsuba mpi", cfg, A, B, [&]() {
            return mpi_multiply::karatsubaMPI(A, B, MPI_COMM_WORLD, cfg.karatsubaSeqBase);
        }, rank));

        if (rank == 0 && cfg.verify) {
            std::cout << "\nVerification:\n";
            bool naiveOk = results[0].result == results[1].result;
            bool karaOk = results[2].result == results[3].result;
            bool crossOk = results[0].result == results[2].result;

            std::cout << "  naive mpi == naive seq:     " << (naiveOk ? "OK" : "MISMATCH") << "\n";
            std::cout << "  karatsuba mpi == kara seq:  " << (karaOk ? "OK" : "MISMATCH") << "\n";
            std::cout << "  karatsuba seq == naive seq: " << (crossOk ? "OK" : "MISMATCH") << "\n";

            std::cout << "\nResult: " << results[0].result.toString() << "\n";
        }
    } else {
        std::function<Polynomial()> fn;
        std::string label;

        if (cfg.algorithm == "naive" && cfg.mode == "seq") {
            label = "naive seq";
            fn = [&]() { return multiply::naiveSeq(A, B); };
        } else if (cfg.algorithm == "naive" && cfg.mode == "mpi") {
            label = "naive mpi";
            fn = [&]() { return mpi_multiply::naiveMPI(A, B, MPI_COMM_WORLD); };
        } else if (cfg.algorithm == "karatsuba" && cfg.mode == "seq") {
            label = "karatsuba seq";
            fn = [&]() { return multiply::karatsubaSeq(A, B, cfg.karatsubaSeqBase); };
        } else if (cfg.algorithm == "karatsuba" && cfg.mode == "mpi") {
            label = "karatsuba mpi";
            fn = [&]() { return mpi_multiply::karatsubaMPI(A, B, MPI_COMM_WORLD, cfg.karatsubaSeqBase); };
        } else {
            if (rank == 0) {
                std::cerr << "Invalid algorithm/mode: " << cfg.algorithm << "/" << cfg.mode << "\n";
            }
            MPI_Finalize();
            return 1;
        }

        auto result = benchmark(label, cfg, A, B, fn, rank);

        if (rank == 0 && cfg.verify) {
            Polynomial ref = multiply::naiveSeq(A, B);
            bool ok = ref == result.result;
            std::cout << "\nVerification vs naive seq: " << (ok ? "OK" : "MISMATCH") << "\n";
        }

        if (rank == 0) {
            std::cout << "\nResult: " << result.result.toString() << "\n";
        }
    }

    MPI_Finalize();
    return 0;
}

BenchResult benchmark(const std::string& label, const Config& cfg,
                      const Polynomial& a, const Polynomial& b,
                      std::function<Polynomial()> fn, int rank) {
    std::vector<double> samples;
    Polynomial result;

    for (int r = 0; r < cfg.repeats; ++r) {
        MPI_Barrier(MPI_COMM_WORLD);
        auto t0 = std::chrono::high_resolution_clock::now();
        Polynomial tmp = fn();
        auto t1 = std::chrono::high_resolution_clock::now();

        double ms = std::chrono::duration<double, std::milli>(t1 - t0).count();
        samples.push_back(ms);

        if (r == 0) result = tmp;
    }

    double avg = 0, minVal = samples[0];
    for (double s : samples) {
        avg += s;
        if (s < minVal) minVal = s;
    }
    avg /= samples.size();

    if (rank == 0) {
        std::cout << std::left << std::setw(24) << label
                  << " | avg: " << std::setw(10) << std::fixed << std::setprecision(3) << avg << " ms"
                  << "  min: " << std::setw(10) << minVal << " ms\n";
    }

    return {result, avg, minVal};
}

Config parseArgs(int argc, char** argv) {
    Config cfg;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];

        if ((arg == "--algorithm" || arg == "-a") && i + 1 < argc) {
            cfg.algorithm = argv[++i];
        } else if ((arg == "--mode" || arg == "-m") && i + 1 < argc) {
            cfg.mode = argv[++i];
        } else if ((arg == "--size" || arg == "-n") && i + 1 < argc) {
            cfg.size = std::stoi(argv[++i]);
        } else if ((arg == "--repeats" || arg == "-r") && i + 1 < argc) {
            cfg.repeats = std::stoi(argv[++i]);
        } else if (arg == "--compare") {
            cfg.compare = true;
        } else if (arg == "--no-verify") {
            cfg.verify = false;
        } else if (arg == "--karatsuba-seq-base" && i + 1 < argc) {
            cfg.karatsubaSeqBase = std::stoi(argv[++i]);
        } else if (arg == "--help" || arg == "-h") {
            int rank;
            MPI_Comm_rank(MPI_COMM_WORLD, &rank);
            if (rank == 0) printHelp();
            MPI_Finalize();
            exit(0);
        }
    }

    return cfg;
}

void printHelp() {
    std::cout << R"(
Usage: mpirun -np <N> ./lab07 [OPTIONS]

Options:
  --algorithm, -a <naive|karatsuba>   Algorithm to use
  --mode, -m <seq|mpi>                Sequential or MPI-distributed
  --size, -n <N>                      Polynomial size (default: 1000)
  --repeats, -r <R>                   Benchmark repeats (default: 3)
  --compare                           Run all 4 variants
  --no-verify                         Skip verification
  --karatsuba-seq-base <B>            Karatsuba base threshold (default: 64)
  --help, -h                          Show this help

Examples:
  mpirun -np 4 ./lab07 --compare --size 5000 --repeats 5
  mpirun -np 4 ./lab07 --algorithm naive --mode mpi --size 10000
)";
}
