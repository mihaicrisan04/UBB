#pragma once

#include "../common/types.hpp"
#include "../common/fitness.hpp"
#include "../common/operators.hpp"
#include "../common/utils.hpp"

#include <thread>
#include <mutex>
#include <atomic>
#include <future>
#include <vector>
#include <algorithm>
#include <numeric>

namespace timetable {

class ThreadedGA {
public:
    ThreadedGA(const ProblemConfig& problem, const GAConfig& ga_config,
               int num_threads = std::thread::hardware_concurrency())
        : problem_(problem),
          ga_config_(ga_config),
          num_threads_(num_threads),
          evaluator_(problem),
          operators_(problem) {}

    // Run the genetic algorithm
    Chromosome run(bool verbose = true) {
        Timer timer;
        timer.start();

        // Initialize population in parallel
        std::vector<Chromosome> population = init_population_parallel();

        // Evaluate initial population
        evaluate_population_parallel(population);
        sort_population(population);

        best_ever_ = population.back();

        if (verbose) {
            print_stats(0, best_ever_.fitness, average_fitness(population), timer.elapsed_ms());
        }

        for (int gen = 1; gen <= ga_config_.max_generations; gen++) {
            // Create next generation
            std::vector<Chromosome> next_gen = evolve_generation_parallel(population);

            // Evaluate new population
            evaluate_population_parallel(next_gen);
            sort_population(next_gen);

            // Elitism: keep best from previous generation
            for (int i = 0; i < ga_config_.elitism_count && i < static_cast<int>(population.size()); i++) {
                if (population[population.size() - 1 - i].fitness > next_gen[i].fitness) {
                    next_gen[i] = population[population.size() - 1 - i];
                }
            }
            sort_population(next_gen);

            population = std::move(next_gen);

            if (population.back().fitness > best_ever_.fitness) {
                best_ever_ = population.back();
            }

            if (verbose && gen % 10 == 0) {
                print_stats(gen, best_ever_.fitness, average_fitness(population), timer.elapsed_ms());
            }

            // Check termination condition
            if (best_ever_.fitness >= ga_config_.target_fitness) {
                if (verbose) {
                    std::cout << "Target fitness reached at generation " << gen << "\n";
                }
                break;
            }
        }

        total_time_ms_ = timer.elapsed_ms();

        if (verbose) {
            std::cout << "\nFinal results:\n";
            std::cout << "Best fitness: " << best_ever_.fitness << "\n";
            std::cout << "Total time: " << total_time_ms_ << " ms\n";
            std::cout << "Threads used: " << num_threads_ << "\n";
        }

        return best_ever_;
    }

    double get_total_time_ms() const { return total_time_ms_; }
    Chromosome get_best() const { return best_ever_; }

private:
    // Initialize population using multiple threads
    std::vector<Chromosome> init_population_parallel() {
        std::vector<Chromosome> population(ga_config_.population_size);
        std::vector<std::future<void>> futures;

        int chunk_size = ga_config_.population_size / num_threads_;

        for (int t = 0; t < num_threads_; t++) {
            int start = t * chunk_size;
            int end = (t == num_threads_ - 1) ? ga_config_.population_size : start + chunk_size;

            futures.push_back(std::async(std::launch::async, [this, &population, start, end, t]() {
                GeneticOperators local_ops(problem_, static_cast<unsigned>(t + std::random_device{}()));
                for (int i = start; i < end; i++) {
                    population[i] = local_ops.create_random();
                }
            }));
        }

        for (auto& f : futures) {
            f.get();
        }

        return population;
    }

    // Evaluate fitness of entire population in parallel
    void evaluate_population_parallel(std::vector<Chromosome>& population) {
        std::vector<std::future<void>> futures;
        int chunk_size = static_cast<int>(population.size()) / num_threads_;

        for (int t = 0; t < num_threads_; t++) {
            int start = t * chunk_size;
            int end = (t == num_threads_ - 1) ? static_cast<int>(population.size()) : start + chunk_size;

            futures.push_back(std::async(std::launch::async, [this, &population, start, end]() {
                for (int i = start; i < end; i++) {
                    population[i].fitness = evaluator_.evaluate(population[i]);
                }
            }));
        }

        for (auto& f : futures) {
            f.get();
        }
    }

    // Create next generation using parallel evolution
    std::vector<Chromosome> evolve_generation_parallel(const std::vector<Chromosome>& population) {
        std::vector<Chromosome> next_gen(ga_config_.population_size);
        std::vector<std::future<void>> futures;
        std::mutex pop_mutex;

        int pairs_per_thread = (ga_config_.population_size / 2) / num_threads_;

        for (int t = 0; t < num_threads_; t++) {
            int start_pair = t * pairs_per_thread;
            int end_pair = (t == num_threads_ - 1) ?
                           (ga_config_.population_size / 2) : start_pair + pairs_per_thread;

            futures.push_back(std::async(std::launch::async,
                [this, &population, &next_gen, &pop_mutex, start_pair, end_pair, t]() {
                    GeneticOperators local_ops(problem_, static_cast<unsigned>(t + std::random_device{}()));
                    std::mt19937 local_rng(static_cast<unsigned>(t + std::random_device{}()));
                    std::uniform_real_distribution<double> prob(0.0, 1.0);

                    for (int p = start_pair; p < end_pair; p++) {
                        // Selection
                        const Chromosome& parent1 = local_ops.tournament_select(population, ga_config_.tournament_size);
                        const Chromosome& parent2 = local_ops.tournament_select(population, ga_config_.tournament_size);

                        Chromosome child1, child2;

                        // Crossover
                        if (prob(local_rng) < ga_config_.crossover_rate) {
                            auto [c1, c2] = local_ops.crossover(parent1, parent2);
                            child1 = std::move(c1);
                            child2 = std::move(c2);
                        } else {
                            child1 = parent1;
                            child2 = parent2;
                        }

                        // Mutation
                        local_ops.mutate(child1, ga_config_.mutation_rate);
                        local_ops.mutate(child2, ga_config_.mutation_rate);

                        // Store children
                        int idx1 = p * 2;
                        int idx2 = p * 2 + 1;

                        std::lock_guard<std::mutex> lock(pop_mutex);
                        if (idx1 < static_cast<int>(next_gen.size())) next_gen[idx1] = std::move(child1);
                        if (idx2 < static_cast<int>(next_gen.size())) next_gen[idx2] = std::move(child2);
                    }
                }));
        }

        for (auto& f : futures) {
            f.get();
        }

        return next_gen;
    }

    void sort_population(std::vector<Chromosome>& population) {
        std::sort(population.begin(), population.end());
    }

    double average_fitness(const std::vector<Chromosome>& population) const {
        double sum = std::accumulate(population.begin(), population.end(), 0.0,
            [](double acc, const Chromosome& c) { return acc + c.fitness; });
        return sum / population.size();
    }

    const ProblemConfig& problem_;
    GAConfig ga_config_;
    int num_threads_;
    FitnessEvaluator evaluator_;
    GeneticOperators operators_;
    Chromosome best_ever_;
    double total_time_ms_ = 0;
};

} // namespace timetable
