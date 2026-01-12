#pragma once

#include "types.hpp"
#include <random>
#include <algorithm>

namespace timetable {

class GeneticOperators {
public:
    GeneticOperators(const ProblemConfig& config, unsigned seed = std::random_device{}())
        : config_(config), rng_(seed) {}

    // Create a random chromosome
    Chromosome create_random() {
        Chromosome c;
        int slot = 0;

        for (const auto& req : config_.requirements) {
            // For each required hour
            for (int h = 0; h < req.hours_per_week; h++) {
                Assignment a;
                a.class_req_id = req.id;
                a.teacher_id = select_random_valid_teacher(req.subject_id);
                a.room_id = select_random_room();
                a.timeslot_idx = (slot++) % config_.total_timeslots();
                c.assignments.push_back(a);
            }
        }

        // Shuffle timeslots for randomness
        std::shuffle(c.assignments.begin(), c.assignments.end(), rng_);
        reassign_timeslots(c);

        return c;
    }

    // Tournament selection
    const Chromosome& tournament_select(const std::vector<Chromosome>& population, int tournament_size) {
        std::uniform_int_distribution<size_t> dist(0, population.size() - 1);

        size_t best_idx = dist(rng_);
        double best_fitness = population[best_idx].fitness;

        for (int i = 1; i < tournament_size; i++) {
            size_t idx = dist(rng_);
            if (population[idx].fitness > best_fitness) {
                best_idx = idx;
                best_fitness = population[idx].fitness;
            }
        }

        return population[best_idx];
    }

    // Single-point crossover
    std::pair<Chromosome, Chromosome> crossover(const Chromosome& parent1, const Chromosome& parent2) {
        if (parent1.assignments.size() != parent2.assignments.size()) {
            return {parent1, parent2};
        }

        size_t n = parent1.assignments.size();
        std::uniform_int_distribution<size_t> dist(1, n - 1);
        size_t crossover_point = dist(rng_);

        Chromosome child1, child2;
        child1.assignments.reserve(n);
        child2.assignments.reserve(n);

        for (size_t i = 0; i < crossover_point; i++) {
            child1.assignments.push_back(parent1.assignments[i]);
            child2.assignments.push_back(parent2.assignments[i]);
        }
        for (size_t i = crossover_point; i < n; i++) {
            child1.assignments.push_back(parent2.assignments[i]);
            child2.assignments.push_back(parent1.assignments[i]);
        }

        return {child1, child2};
    }

    // Mutation: randomly change teacher, room, or timeslot
    void mutate(Chromosome& chromosome, double mutation_rate) {
        std::uniform_real_distribution<double> prob(0.0, 1.0);
        std::uniform_int_distribution<int> mutation_type(0, 2);

        for (auto& a : chromosome.assignments) {
            if (prob(rng_) < mutation_rate) {
                switch (mutation_type(rng_)) {
                    case 0:  // Change teacher
                        a.teacher_id = select_random_valid_teacher(get_subject_for_req(a.class_req_id));
                        break;
                    case 1:  // Change room
                        a.room_id = select_random_room();
                        break;
                    case 2:  // Change timeslot
                        a.timeslot_idx = select_random_timeslot();
                        break;
                }
            }
        }
    }

    // Seed the RNG (for thread safety)
    void seed(unsigned s) { rng_.seed(s); }

private:
    Id select_random_valid_teacher(Id subject_id) {
        std::vector<Id> valid;
        for (const auto& t : config_.teachers) {
            if (t.subjects.count(subject_id) > 0) {
                valid.push_back(t.id);
            }
        }

        if (valid.empty()) {
            // Fallback: return any teacher
            std::uniform_int_distribution<size_t> dist(0, config_.teachers.size() - 1);
            return config_.teachers[dist(rng_)].id;
        }

        std::uniform_int_distribution<size_t> dist(0, valid.size() - 1);
        return valid[dist(rng_)];
    }

    Id select_random_room() {
        std::uniform_int_distribution<size_t> dist(0, config_.rooms.size() - 1);
        return config_.rooms[dist(rng_)].id;
    }

    int select_random_timeslot() {
        std::uniform_int_distribution<int> dist(0, config_.total_timeslots() - 1);
        return dist(rng_);
    }

    Id get_subject_for_req(Id req_id) {
        for (const auto& req : config_.requirements) {
            if (req.id == req_id) return req.subject_id;
        }
        return 0;
    }

    void reassign_timeslots(Chromosome& c) {
        for (size_t i = 0; i < c.assignments.size(); i++) {
            c.assignments[i].timeslot_idx = static_cast<int>(i) % config_.total_timeslots();
        }
    }

    const ProblemConfig& config_;
    std::mt19937 rng_;
};

} // namespace timetable
