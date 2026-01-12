#include "ga_mpi.hpp"
#include "../common/problem_generator.hpp"
#include <iostream>
#include <string>

void print_usage() {
    std::cout << "Usage: mpirun -np <N> timetable_mpi [options]\n"
              << "Options:\n"
              << "  --pop N          Population size (default: 100)\n"
              << "  --gen N          Max generations (default: 500)\n"
              << "  --migration N    Migration interval (default: 50)\n"
              << "  --teachers N     Number of teachers (default: 10)\n"
              << "  --rooms N        Number of rooms (default: 8)\n"
              << "  --groups N       Number of student groups (default: 6)\n"
              << "  --subjects N     Number of subjects (default: 8)\n"
              << "  --classes N      Classes per group (default: 5)\n"
              << "  --small          Use small test problem\n"
              << "  --quiet          Minimal output\n";
}

int main(int argc, char* argv[]) {
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    bool use_small = false;
    bool quiet = false;

    // Problem parameters
    int num_teachers = 10;
    int num_rooms = 8;
    int num_groups = 6;
    int num_subjects = 8;
    int classes_per_group = 5;

    // GA parameters
    timetable::GAConfig ga_config;
    ga_config.population_size = 100;
    ga_config.max_generations = 500;
    ga_config.migration_interval = 50;
    ga_config.migration_count = 5;

    // Parse arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            if (rank == 0) print_usage();
            MPI_Finalize();
            return 0;
        } else if (arg == "--pop" && i + 1 < argc) {
            ga_config.population_size = std::stoi(argv[++i]);
        } else if (arg == "--gen" && i + 1 < argc) {
            ga_config.max_generations = std::stoi(argv[++i]);
        } else if (arg == "--migration" && i + 1 < argc) {
            ga_config.migration_interval = std::stoi(argv[++i]);
        } else if (arg == "--teachers" && i + 1 < argc) {
            num_teachers = std::stoi(argv[++i]);
        } else if (arg == "--rooms" && i + 1 < argc) {
            num_rooms = std::stoi(argv[++i]);
        } else if (arg == "--groups" && i + 1 < argc) {
            num_groups = std::stoi(argv[++i]);
        } else if (arg == "--subjects" && i + 1 < argc) {
            num_subjects = std::stoi(argv[++i]);
        } else if (arg == "--classes" && i + 1 < argc) {
            classes_per_group = std::stoi(argv[++i]);
        } else if (arg == "--small") {
            use_small = true;
        } else if (arg == "--quiet") {
            quiet = true;
        }
    }

    // Generate problem (same seed on all processes)
    timetable::ProblemConfig problem;
    if (use_small) {
        problem = timetable::ProblemGenerator::generate_small();
        if (rank == 0) std::cout << "Using small test problem\n";
    } else {
        timetable::ProblemGenerator gen(42);  // Fixed seed for reproducibility
        problem = gen.generate(num_teachers, num_rooms, num_groups, num_subjects, classes_per_group);
        if (rank == 0) {
            std::cout << "Generated problem: "
                      << num_teachers << " teachers, "
                      << num_rooms << " rooms, "
                      << num_groups << " groups, "
                      << num_subjects << " subjects\n";
        }
    }

    // Count total assignments
    int total_assignments = 0;
    for (const auto& req : problem.requirements) {
        total_assignments += req.hours_per_week;
    }

    if (rank == 0) {
        std::cout << "Total class assignments to schedule: " << total_assignments << "\n";
        std::cout << "MPI processes: " << size << "\n";
        std::cout << "Population per process: " << ga_config.population_size / size << "\n";
        std::cout << "Migration interval: " << ga_config.migration_interval << " generations\n\n";
    }

    MPI_Barrier(MPI_COMM_WORLD);

    // Run the distributed GA
    timetable::MPIGA ga(problem, ga_config, MPI_COMM_WORLD);
    auto best = ga.run(!quiet);

    if (rank == 0 && !quiet) {
        std::cout << "\nBest timetable found:\n";
        timetable::print_chromosome(best, problem);
    }

    MPI_Finalize();
    return 0;
}
