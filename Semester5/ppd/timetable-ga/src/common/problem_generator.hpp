#pragma once

#include "types.hpp"
#include <random>
#include <sstream>

namespace timetable {

class ProblemGenerator {
public:
    explicit ProblemGenerator(unsigned seed = std::random_device{}())
        : rng_(seed) {}

    // Generate a random problem instance
    ProblemConfig generate(int num_teachers, int num_rooms, int num_groups,
                           int num_subjects, int classes_per_group) {
        ProblemConfig config;
        config.days_per_week = 5;
        config.hours_per_day = 8;

        // Generate subjects
        for (int i = 0; i < num_subjects; i++) {
            Subject s;
            s.id = static_cast<Id>(i);
            s.name = "Subject_" + std::to_string(i);
            config.subjects.push_back(s);
        }

        // Generate teachers (each can teach 1-3 subjects)
        std::uniform_int_distribution<int> subj_count_dist(1, std::min(3, num_subjects));
        std::uniform_int_distribution<int> subj_dist(0, num_subjects - 1);

        for (int i = 0; i < num_teachers; i++) {
            Teacher t;
            t.id = static_cast<Id>(i);
            t.name = "Teacher_" + std::to_string(i);

            int subj_count = subj_count_dist(rng_);
            while (static_cast<int>(t.subjects.size()) < subj_count) {
                t.subjects.insert(static_cast<Id>(subj_dist(rng_)));
            }
            config.teachers.push_back(t);
        }

        // Generate rooms with varying capacity
        std::uniform_int_distribution<int> cap_dist(20, 100);
        for (int i = 0; i < num_rooms; i++) {
            Room r;
            r.id = static_cast<Id>(i);
            r.name = "Room_" + std::to_string(i);
            r.capacity = cap_dist(rng_);
            config.rooms.push_back(r);
        }

        // Generate student groups
        std::uniform_int_distribution<int> size_dist(15, 35);
        for (int i = 0; i < num_groups; i++) {
            StudentGroup g;
            g.id = static_cast<Id>(i);
            g.name = "Group_" + std::to_string(i);
            g.size = size_dist(rng_);
            config.groups.push_back(g);
        }

        // Generate class requirements
        Id req_id = 0;
        std::uniform_int_distribution<int> hours_dist(1, 3);

        for (int g = 0; g < num_groups; g++) {
            std::set<int> used_subjects;

            for (int c = 0; c < classes_per_group && static_cast<int>(used_subjects.size()) < num_subjects; c++) {
                int subj;
                do {
                    subj = subj_dist(rng_);
                } while (used_subjects.count(subj) > 0);
                used_subjects.insert(subj);

                ClassRequirement req;
                req.id = req_id++;
                req.subject_id = static_cast<Id>(subj);
                req.group_id = static_cast<Id>(g);
                req.hours_per_week = hours_dist(rng_);
                config.requirements.push_back(req);
            }
        }

        return config;
    }

    // Generate a small test problem
    static ProblemConfig generate_small() {
        ProblemConfig config;
        config.days_per_week = 5;
        config.hours_per_day = 6;

        // 3 subjects
        config.subjects = {
            {0, "Math"},
            {1, "Physics"},
            {2, "Chemistry"}
        };

        // 4 teachers
        config.teachers = {
            {0, "Prof. Smith", {0, 1}},      // teaches Math and Physics
            {1, "Prof. Jones", {1, 2}},      // teaches Physics and Chemistry
            {2, "Prof. Brown", {0}},         // teaches Math
            {3, "Prof. Davis", {2}}          // teaches Chemistry
        };

        // 3 rooms
        config.rooms = {
            {0, "Room 101", 30},
            {1, "Room 102", 25},
            {2, "Lab A", 20}
        };

        // 2 student groups
        config.groups = {
            {0, "Group A", 25},
            {1, "Group B", 20}
        };

        // Class requirements
        config.requirements = {
            {0, 0, 0, 2},  // Group A needs 2h of Math
            {1, 1, 0, 2},  // Group A needs 2h of Physics
            {2, 2, 0, 1},  // Group A needs 1h of Chemistry
            {3, 0, 1, 2},  // Group B needs 2h of Math
            {4, 1, 1, 1},  // Group B needs 1h of Physics
            {5, 2, 1, 2}   // Group B needs 2h of Chemistry
        };

        return config;
    }

private:
    std::mt19937 rng_;
};

} // namespace timetable
