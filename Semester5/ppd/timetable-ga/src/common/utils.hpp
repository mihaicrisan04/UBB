#pragma once

#include "types.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <map>

namespace timetable {

class Timer {
public:
    void start() {
        start_time_ = std::chrono::high_resolution_clock::now();
    }

    double elapsed_ms() const {
        auto now = std::chrono::high_resolution_clock::now();
        return std::chrono::duration<double, std::milli>(now - start_time_).count();
    }

    double elapsed_s() const {
        return elapsed_ms() / 1000.0;
    }

private:
    std::chrono::high_resolution_clock::time_point start_time_;
};

inline void print_chromosome(const Chromosome& c, const ProblemConfig& config) {
    std::cout << "Timetable (fitness: " << std::fixed << std::setprecision(4)
              << c.fitness << ")\n";
    std::cout << std::string(60, '-') << "\n";

    // Build lookup maps
    std::map<Id, std::string> teacher_names;
    std::map<Id, std::string> room_names;
    std::map<Id, std::string> group_names;
    std::map<Id, std::string> subject_names;
    std::map<Id, Id> req_to_subject;
    std::map<Id, Id> req_to_group;

    for (const auto& t : config.teachers) teacher_names[t.id] = t.name;
    for (const auto& r : config.rooms) room_names[r.id] = r.name;
    for (const auto& g : config.groups) group_names[g.id] = g.name;
    for (const auto& s : config.subjects) subject_names[s.id] = s.name;
    for (const auto& req : config.requirements) {
        req_to_subject[req.id] = req.subject_id;
        req_to_group[req.id] = req.group_id;
    }

    // Group by timeslot
    std::map<int, std::vector<const Assignment*>> by_slot;
    for (const auto& a : c.assignments) {
        by_slot[a.timeslot_idx].push_back(&a);
    }

    const char* days[] = {"Mon", "Tue", "Wed", "Thu", "Fri"};

    for (const auto& [slot, assignments] : by_slot) {
        Timeslot ts = Timeslot::from_index(slot, config.hours_per_day);
        std::cout << days[ts.day] << " " << std::setw(2) << (8 + ts.hour) << ":00 | ";

        for (const auto* a : assignments) {
            Id subj = req_to_subject[a->class_req_id];
            Id grp = req_to_group[a->class_req_id];
            std::cout << subject_names[subj] << " ("
                      << group_names[grp] << ", "
                      << teacher_names[a->teacher_id] << ", "
                      << room_names[a->room_id] << ") ";
        }
        std::cout << "\n";
    }
    std::cout << std::string(60, '-') << "\n";
}

inline void print_stats(int generation, double best_fitness, double avg_fitness, double time_ms) {
    std::cout << "Gen " << std::setw(5) << generation
              << " | Best: " << std::fixed << std::setprecision(4) << best_fitness
              << " | Avg: " << std::setprecision(4) << avg_fitness
              << " | Time: " << std::setprecision(1) << time_ms << " ms\n";
}

} // namespace timetable
