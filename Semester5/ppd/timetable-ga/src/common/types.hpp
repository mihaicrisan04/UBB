#pragma once

#include <cstdint>
#include <string>
#include <vector>
#include <set>

namespace timetable {

using Id = uint32_t;

struct Teacher {
    Id id;
    std::string name;
    std::set<Id> subjects;  // subjects this teacher can teach
};

struct Room {
    Id id;
    std::string name;
    int capacity;
};

struct StudentGroup {
    Id id;
    std::string name;
    int size;
};

struct Subject {
    Id id;
    std::string name;
};

struct ClassRequirement {
    Id id;
    Id subject_id;
    Id group_id;
    int hours_per_week;  // how many slots needed
};

struct Timeslot {
    int day;   // 0-4 (Mon-Fri)
    int hour;  // 0-7 (8 hours per day)

    int to_index(int hours_per_day = 8) const {
        return day * hours_per_day + hour;
    }

    static Timeslot from_index(int idx, int hours_per_day = 8) {
        return {idx / hours_per_day, idx % hours_per_day};
    }
};

// A single assignment in the timetable
struct Assignment {
    Id class_req_id;
    Id teacher_id;
    Id room_id;
    int timeslot_idx;  // encoded as day * hours_per_day + hour
};

// Configuration for the problem
struct ProblemConfig {
    int days_per_week = 5;
    int hours_per_day = 8;

    std::vector<Teacher> teachers;
    std::vector<Room> rooms;
    std::vector<StudentGroup> groups;
    std::vector<Subject> subjects;
    std::vector<ClassRequirement> requirements;

    int total_timeslots() const { return days_per_week * hours_per_day; }
};

// A chromosome represents a complete timetable solution
struct Chromosome {
    std::vector<Assignment> assignments;
    double fitness = 0.0;

    bool operator<(const Chromosome& other) const {
        return fitness < other.fitness;  // higher fitness is better
    }
};

// GA parameters
struct GAConfig {
    int population_size = 100;
    int max_generations = 1000;
    double crossover_rate = 0.8;
    double mutation_rate = 0.1;
    int elitism_count = 2;
    int tournament_size = 5;
    double target_fitness = 1.0;  // stop if reached

    // For distributed version
    int migration_interval = 50;
    int migration_count = 5;
};

} // namespace timetable
