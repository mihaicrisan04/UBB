#pragma once

#include "../common/types.hpp"
#include "../common/fitness.hpp"
#include "../common/operators.hpp"
#include "../common/utils.hpp"

#include <mpi.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cstring>

namespace timetable {

// Serialization helpers for MPI communication
class ChromosomeSerializer {
public:
    // Get size needed for serialization
    static size_t serialized_size(const Chromosome& c) {
        // Format: num_assignments (int) + assignments[] + fitness (double)
        return sizeof(int) + c.assignments.size() * sizeof(Assignment) + sizeof(double);
    }

    // Serialize to buffer
    static void serialize(const Chromosome& c, std::vector<char>& buffer) {
        buffer.resize(serialized_size(c));
        char* ptr = buffer.data();

        int num = static_cast<int>(c.assignments.size());
        std::memcpy(ptr, &num, sizeof(int));
        ptr += sizeof(int);

        std::memcpy(ptr, c.assignments.data(), num * sizeof(Assignment));
        ptr += num * sizeof(Assignment);

        std::memcpy(ptr, &c.fitness, sizeof(double));
    }

    // Deserialize from buffer
    static Chromosome deserialize(const char* buffer) {
        Chromosome c;
        const char* ptr = buffer;

        int num;
        std::memcpy(&num, ptr, sizeof(int));
        ptr += sizeof(int);

        c.assignments.resize(num);
        std::memcpy(c.assignments.data(), ptr, num * sizeof(Assignment));
        ptr += num * sizeof(Assignment);

        std::memcpy(&c.fitness, ptr, sizeof(double));

        return c;
    }
};

class MPIGA {
public:
    MPIGA(const ProblemConfig& problem, const GAConfig& ga_config, MPI_Comm comm = MPI_COMM_WORLD)
        : problem_(problem),
          ga_config_(ga_config),
          comm_(comm),
          evaluator_(problem) {
        MPI_Comm_rank(comm_, &rank_);
        MPI_Comm_size(comm_, &size_);

        // Each process gets a portion of the population
        local_pop_size_ = ga_config_.population_size / size_;
        if (rank_ < ga_config_.population_size % size_) {
            local_pop_size_++;
        }
    }

    Chromosome run(bool verbose = true) {
        Timer timer;
        timer.start();

        // Initialize local population
        GeneticOperators local_ops(problem_, static_cast<unsigned>(rank_ + std::random_device{}()));

        std::vector<Chromosome> population(local_pop_size_);
        for (auto& c : population) {
            c = local_ops.create_random();
            c.fitness = evaluator_.evaluate(c);
        }

        sort_population(population);
        local_best_ = population.back();

        // Find global best
        Chromosome global_best = find_global_best();

        if (rank_ == 0 && verbose) {
            print_stats(0, global_best.fitness, 0, timer.elapsed_ms());
        }

        for (int gen = 1; gen <= ga_config_.max_generations; gen++) {
            // Evolution step
            std::vector<Chromosome> next_gen = evolve_generation(population, local_ops);

            // Evaluate new population
            for (auto& c : next_gen) {
                c.fitness = evaluator_.evaluate(c);
            }
            sort_population(next_gen);

            // Elitism
            for (int i = 0; i < ga_config_.elitism_count && i < static_cast<int>(population.size()); i++) {
                if (population[population.size() - 1 - i].fitness > next_gen[i].fitness) {
                    next_gen[i] = population[population.size() - 1 - i];
                }
            }
            sort_population(next_gen);

            population = std::move(next_gen);

            if (population.back().fitness > local_best_.fitness) {
                local_best_ = population.back();
            }

            // Migration between islands
            if (gen % ga_config_.migration_interval == 0) {
                migrate(population);
                sort_population(population);
            }

            // Find global best periodically
            if (gen % 10 == 0 || gen == ga_config_.max_generations) {
                global_best = find_global_best();

                if (rank_ == 0 && verbose) {
                    print_stats(gen, global_best.fitness, 0, timer.elapsed_ms());
                }

                // Check termination
                if (global_best.fitness >= ga_config_.target_fitness) {
                    if (rank_ == 0 && verbose) {
                        std::cout << "Target fitness reached at generation " << gen << "\n";
                    }
                    break;
                }
            }
        }

        total_time_ms_ = timer.elapsed_ms();

        // Final global best
        global_best = find_global_best();

        if (rank_ == 0 && verbose) {
            std::cout << "\nFinal results:\n";
            std::cout << "Best fitness: " << global_best.fitness << "\n";
            std::cout << "Total time: " << total_time_ms_ << " ms\n";
            std::cout << "MPI processes: " << size_ << "\n";
        }

        return global_best;
    }

    double get_total_time_ms() const { return total_time_ms_; }
    int get_rank() const { return rank_; }

private:
    std::vector<Chromosome> evolve_generation(const std::vector<Chromosome>& population,
                                               GeneticOperators& ops) {
        std::vector<Chromosome> next_gen;
        next_gen.reserve(local_pop_size_);

        std::mt19937 rng(static_cast<unsigned>(rank_ + std::random_device{}()));
        std::uniform_real_distribution<double> prob(0.0, 1.0);

        while (static_cast<int>(next_gen.size()) < local_pop_size_) {
            // Selection
            const Chromosome& parent1 = ops.tournament_select(population, ga_config_.tournament_size);
            const Chromosome& parent2 = ops.tournament_select(population, ga_config_.tournament_size);

            Chromosome child1, child2;

            // Crossover
            if (prob(rng) < ga_config_.crossover_rate) {
                auto [c1, c2] = ops.crossover(parent1, parent2);
                child1 = std::move(c1);
                child2 = std::move(c2);
            } else {
                child1 = parent1;
                child2 = parent2;
            }

            // Mutation
            ops.mutate(child1, ga_config_.mutation_rate);
            ops.mutate(child2, ga_config_.mutation_rate);

            next_gen.push_back(std::move(child1));
            if (static_cast<int>(next_gen.size()) < local_pop_size_) {
                next_gen.push_back(std::move(child2));
            }
        }

        return next_gen;
    }

    // Ring topology migration
    void migrate(std::vector<Chromosome>& population) {
        int dest = (rank_ + 1) % size_;
        int src = (rank_ - 1 + size_) % size_;

        // Prepare best individuals to send
        std::vector<char> send_buffer;
        std::vector<char> recv_buffer;

        // Get the best individuals to send
        int send_count = std::min(ga_config_.migration_count, static_cast<int>(population.size()));
        std::vector<Chromosome> to_send(population.end() - send_count, population.end());

        // Serialize
        size_t single_size = ChromosomeSerializer::serialized_size(to_send[0]);
        send_buffer.resize(single_size * send_count);
        recv_buffer.resize(single_size * send_count);

        for (int i = 0; i < send_count; i++) {
            std::vector<char> temp;
            ChromosomeSerializer::serialize(to_send[i], temp);
            std::memcpy(send_buffer.data() + i * single_size, temp.data(), single_size);
        }

        // Exchange with neighbors
        MPI_Sendrecv(send_buffer.data(), static_cast<int>(send_buffer.size()), MPI_CHAR, dest, 0,
                     recv_buffer.data(), static_cast<int>(recv_buffer.size()), MPI_CHAR, src, 0,
                     comm_, MPI_STATUS_IGNORE);

        // Deserialize received individuals and integrate into population
        for (int i = 0; i < send_count; i++) {
            Chromosome received = ChromosomeSerializer::deserialize(recv_buffer.data() + i * single_size);
            // Replace worst individuals
            if (received.fitness > population[i].fitness) {
                population[i] = std::move(received);
            }
        }
    }

    // Find the global best across all processes
    Chromosome find_global_best() {
        // Serialize local best
        std::vector<char> local_buffer;
        ChromosomeSerializer::serialize(local_best_, local_buffer);

        int buffer_size = static_cast<int>(local_buffer.size());

        // Broadcast buffer size (should be same for all, but ensure)
        MPI_Bcast(&buffer_size, 1, MPI_INT, 0, comm_);

        // Gather all local bests to rank 0
        std::vector<char> all_buffers;
        if (rank_ == 0) {
            all_buffers.resize(buffer_size * size_);
        }

        MPI_Gather(local_buffer.data(), buffer_size, MPI_CHAR,
                   all_buffers.data(), buffer_size, MPI_CHAR,
                   0, comm_);

        Chromosome global_best = local_best_;

        if (rank_ == 0) {
            for (int i = 0; i < size_; i++) {
                Chromosome c = ChromosomeSerializer::deserialize(all_buffers.data() + i * buffer_size);
                if (c.fitness > global_best.fitness) {
                    global_best = c;
                }
            }
        }

        // Broadcast the global best back to all processes
        std::vector<char> best_buffer;
        if (rank_ == 0) {
            ChromosomeSerializer::serialize(global_best, best_buffer);
        } else {
            best_buffer.resize(buffer_size);
        }

        MPI_Bcast(best_buffer.data(), buffer_size, MPI_CHAR, 0, comm_);

        if (rank_ != 0) {
            global_best = ChromosomeSerializer::deserialize(best_buffer.data());
        }

        return global_best;
    }

    void sort_population(std::vector<Chromosome>& population) {
        std::sort(population.begin(), population.end());
    }

    const ProblemConfig& problem_;
    GAConfig ga_config_;
    MPI_Comm comm_;
    int rank_;
    int size_;
    int local_pop_size_;
    FitnessEvaluator evaluator_;
    Chromosome local_best_;
    double total_time_ms_ = 0;
};

} // namespace timetable
