#include "ga_threaded.hpp"
#include "../common/problem_generator.hpp"
#include <iostream>
#include <string>
#include <cstdlib>

void print_usage() {
    std::cout << "Usage: timetable_threaded [options]\n"
              << "Options:\n"
              << "  --threads N      Number of threads (default: hardware concurrency)\n"
              << "  --pop N          Population size (default: 100)\n"
              << "  --gen N          Max generations (default: 500)\n"
              << "  --teachers N     Number of teachers (default: 10)\n"
              << "  --rooms N        Number of rooms (default: 8)\n"
              << "  --groups N       Number of student groups (default: 6)\n"
              << "  --subjects N     Number of subjects (default: 8)\n"
              << "  --classes N      Classes per group (default: 5)\n"
              << "  --small          Use small test problem\n"
              << "  --quiet          Minimal output\n"
              << "  --benchmark      Run benchmark with different thread counts\n";
}

void run_benchmark(const timetable::ProblemConfig& problem, const timetable::GAConfig& ga_config) {
    std::cout << "\n=== Benchmark Mode ===\n";
    std::cout << "Population: " << ga_config.population_size
              << ", Generations: " << ga_config.max_generations << "\n\n";

    std::vector<int> thread_counts = {1, 2, 4, 8};
    int max_threads = static_cast<int>(std::thread::hardware_concurrency());

    // Filter to available threads
    thread_counts.erase(
        std::remove_if(thread_counts.begin(), thread_counts.end(),
            [max_threads](int t) { return t > max_threads; }),
        thread_counts.end());

    if (std::find(thread_counts.begin(), thread_counts.end(), max_threads) == thread_counts.end()) {
        thread_counts.push_back(max_threads);
    }

    std::cout << "Threads | Time (ms) | Speedup | Best Fitness\n";
    std::cout << "--------|-----------|---------|-------------\n";

    double baseline_time = 0;

    for (int num_threads : thread_counts) {
        timetable::ThreadedGA ga(problem, ga_config, num_threads);
        auto best = ga.run(false);

        double time_ms = ga.get_total_time_ms();
        if (num_threads == 1) baseline_time = time_ms;

        double speedup = baseline_time / time_ms;

        std::cout << std::setw(7) << num_threads << " | "
                  << std::setw(9) << std::fixed << std::setprecision(1) << time_ms << " | "
                  << std::setw(7) << std::setprecision(2) << speedup << " | "
                  << std::setprecision(4) << best.fitness << "\n";
    }
}

int main(int argc, char* argv[]) {
    int num_threads = static_cast<int>(std::thread::hardware_concurrency());
    bool use_small = false;
    bool quiet = false;
    bool benchmark = false;

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

    // Parse arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            print_usage();
            return 0;
        } else if (arg == "--threads" && i + 1 < argc) {
            num_threads = std::stoi(argv[++i]);
        } else if (arg == "--pop" && i + 1 < argc) {
            ga_config.population_size = std::stoi(argv[++i]);
        } else if (arg == "--gen" && i + 1 < argc) {
            ga_config.max_generations = std::stoi(argv[++i]);
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
        } else if (arg == "--benchmark") {
            benchmark = true;
        }
    }

    // Generate or load problem
    timetable::ProblemConfig problem;
    if (use_small) {
        problem = timetable::ProblemGenerator::generate_small();
        std::cout << "Using small test problem\n";
    } else {
        timetable::ProblemGenerator gen;
        problem = gen.generate(num_teachers, num_rooms, num_groups, num_subjects, classes_per_group);
        std::cout << "Generated problem: "
                  << num_teachers << " teachers, "
                  << num_rooms << " rooms, "
                  << num_groups << " groups, "
                  << num_subjects << " subjects\n";
    }

    // Count total assignments
    int total_assignments = 0;
    for (const auto& req : problem.requirements) {
        total_assignments += req.hours_per_week;
    }
    std::cout << "Total class assignments to schedule: " << total_assignments << "\n\n";

    if (benchmark) {
        run_benchmark(problem, ga_config);
    } else {
        std::cout << "Running with " << num_threads << " threads\n";
        std::cout << "Population: " << ga_config.population_size
                  << ", Max generations: " << ga_config.max_generations << "\n\n";

        timetable::ThreadedGA ga(problem, ga_config, num_threads);
        auto best = ga.run(!quiet);

        if (!quiet) {
            std::cout << "\nBest timetable found:\n";
            timetable::print_chromosome(best, problem);
        }
    }

    return 0;
}
