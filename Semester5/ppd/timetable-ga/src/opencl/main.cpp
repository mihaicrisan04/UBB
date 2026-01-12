#include "ga_opencl.hpp"
#include "../common/problem_generator.hpp"
#include <iostream>
#include <string>

void print_usage() {
    std::cout << "Usage: timetable_opencl [options]\n"
              << "Options:\n"
              << "  --pop N          Population size (default: 100)\n"
              << "  --gen N          Max generations (default: 500)\n"
              << "  --teachers N     Number of teachers (default: 10)\n"
              << "  --rooms N        Number of rooms (default: 8)\n"
              << "  --groups N       Number of student groups (default: 6)\n"
              << "  --subjects N     Number of subjects (default: 8)\n"
              << "  --classes N      Classes per group (default: 5)\n"
              << "  --small          Use small test problem\n"
              << "  --quiet          Minimal output\n";
}

int main(int argc, char* argv[]) {
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

    // Parse arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            print_usage();
            return 0;
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
        }
    }

    // Generate problem
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

    try {
        std::cout << "Initializing OpenCL...\n";
        timetable::OpenCLGA ga(problem, ga_config);
        std::cout << "Using device: " << ga.get_device_name() << "\n\n";

        auto best = ga.run(!quiet);

        if (!quiet) {
            std::cout << "\nBest timetable found:\n";
            timetable::print_chromosome(best, problem);
        }
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}
