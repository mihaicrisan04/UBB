#pragma once

#include "../common/types.hpp"
#include "../common/fitness.hpp"
#include "../common/operators.hpp"
#include "../common/utils.hpp"

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <stdexcept>

namespace timetable {

class OpenCLGA {
public:
    OpenCLGA(const ProblemConfig& problem, const GAConfig& ga_config)
        : problem_(problem),
          ga_config_(ga_config),
          evaluator_(problem),
          operators_(problem) {
        init_opencl();
        prepare_problem_data();
    }

    ~OpenCLGA() {
        cleanup_opencl();
    }

    Chromosome run(bool verbose = true) {
        Timer timer;
        timer.start();

        // Initialize population on CPU
        std::vector<Chromosome> population(ga_config_.population_size);
        for (auto& c : population) {
            c = operators_.create_random();
        }

        // Evaluate using OpenCL
        evaluate_population_opencl(population);
        sort_population(population);

        best_ever_ = population.back();

        if (verbose) {
            print_stats(0, best_ever_.fitness, average_fitness(population), timer.elapsed_ms());
        }

        for (int gen = 1; gen <= ga_config_.max_generations; gen++) {
            // Evolution on CPU (crossover, mutation)
            std::vector<Chromosome> next_gen = evolve_generation(population);

            // Evaluate using OpenCL
            evaluate_population_opencl(next_gen);
            sort_population(next_gen);

            // Elitism
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
            std::cout << "Device: " << device_name_ << "\n";
        }

        return best_ever_;
    }

    double get_total_time_ms() const { return total_time_ms_; }
    Chromosome get_best() const { return best_ever_; }
    std::string get_device_name() const { return device_name_; }

private:
    void init_opencl() {
        cl_int err;

        // Get platforms
        cl_uint num_platforms;
        err = clGetPlatformIDs(0, nullptr, &num_platforms);
        if (err != CL_SUCCESS || num_platforms == 0) {
            throw std::runtime_error("No OpenCL platforms found");
        }

        std::vector<cl_platform_id> platforms(num_platforms);
        clGetPlatformIDs(num_platforms, platforms.data(), nullptr);

        // Find a GPU device (prefer Apple GPU on M1)
        device_ = nullptr;
        for (auto platform : platforms) {
            cl_uint num_devices;
            err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 0, nullptr, &num_devices);
            if (err == CL_SUCCESS && num_devices > 0) {
                std::vector<cl_device_id> devices(num_devices);
                clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, num_devices, devices.data(), nullptr);
                device_ = devices[0];
                break;
            }
        }

        // Fallback to CPU if no GPU
        if (device_ == nullptr) {
            for (auto platform : platforms) {
                cl_uint num_devices;
                err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 0, nullptr, &num_devices);
                if (err == CL_SUCCESS && num_devices > 0) {
                    std::vector<cl_device_id> devices(num_devices);
                    clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, num_devices, devices.data(), nullptr);
                    device_ = devices[0];
                    break;
                }
            }
        }

        if (device_ == nullptr) {
            throw std::runtime_error("No OpenCL device found");
        }

        // Get device name
        char name_buf[256];
        clGetDeviceInfo(device_, CL_DEVICE_NAME, sizeof(name_buf), name_buf, nullptr);
        device_name_ = name_buf;

        // Create context and command queue
        context_ = clCreateContext(nullptr, 1, &device_, nullptr, nullptr, &err);
        if (err != CL_SUCCESS) {
            throw std::runtime_error("Failed to create OpenCL context");
        }

        // Use deprecated function for compatibility with older OpenCL versions
        #pragma clang diagnostic push
        #pragma clang diagnostic ignored "-Wdeprecated-declarations"
        queue_ = clCreateCommandQueue(context_, device_, 0, &err);
        #pragma clang diagnostic pop
        if (err != CL_SUCCESS) {
            throw std::runtime_error("Failed to create command queue");
        }

        // Load and compile kernel
        load_kernel();
    }

    void load_kernel() {
        // Embedded kernel source (using float for Apple Silicon compatibility)
        const char* kernel_source = R"(
typedef struct {
    uint class_req_id;
    uint teacher_id;
    uint room_id;
    int timeslot_idx;
} Assignment;

typedef struct {
    uint id;
    uint subject_id;
    uint group_id;
    int hours_per_week;
} ClassRequirement;

__kernel void evaluate_fitness(
    __global const Assignment* assignments,
    __global const int* assignment_offsets,
    __global const int* assignment_counts,
    __global const ClassRequirement* requirements,
    const int num_requirements,
    __global const uint* teacher_subjects,
    __global const int* teacher_subject_offsets,
    __global const int* teacher_subject_counts,
    const int num_teachers,
    __global const int* room_capacities,
    const int num_rooms,
    __global const int* group_sizes,
    const int num_groups,
    const int total_timeslots,
    __global float* fitness_out)
{
    int gid = get_global_id(0);
    int start = assignment_offsets[gid];
    int count = assignment_counts[gid];
    int total_violations = 0;

    for (int slot = 0; slot < total_timeslots; slot++) {
        int teacher_count[64];
        int room_count[64];
        int group_count[64];
        for (int x = 0; x < 64; x++) {
            teacher_count[x] = 0;
            room_count[x] = 0;
            group_count[x] = 0;
        }

        for (int i = 0; i < count; i++) {
            Assignment a = assignments[start + i];
            if (a.timeslot_idx == slot) {
                if (a.teacher_id < 64) teacher_count[a.teacher_id]++;
                if (a.room_id < 64) room_count[a.room_id]++;
                for (int r = 0; r < num_requirements; r++) {
                    if (requirements[r].id == a.class_req_id) {
                        uint grp = requirements[r].group_id;
                        if (grp < 64) group_count[grp]++;
                        break;
                    }
                }
            }
        }

        for (int t = 0; t < 64 && t < num_teachers; t++) {
            if (teacher_count[t] > 1) total_violations += teacher_count[t] - 1;
        }
        for (int r = 0; r < 64 && r < num_rooms; r++) {
            if (room_count[r] > 1) total_violations += room_count[r] - 1;
        }
        for (int g = 0; g < 64 && g < num_groups; g++) {
            if (group_count[g] > 1) total_violations += group_count[g] - 1;
        }
    }

    for (int i = 0; i < count; i++) {
        Assignment a = assignments[start + i];
        uint subject = 0;
        for (int r = 0; r < num_requirements; r++) {
            if (requirements[r].id == a.class_req_id) {
                subject = requirements[r].subject_id;
                break;
            }
        }
        bool can_teach = false;
        if (a.teacher_id < (uint)num_teachers) {
            int t_start = teacher_subject_offsets[a.teacher_id];
            int t_count = teacher_subject_counts[a.teacher_id];
            for (int s = 0; s < t_count; s++) {
                if (teacher_subjects[t_start + s] == subject) {
                    can_teach = true;
                    break;
                }
            }
        }
        if (!can_teach) total_violations++;
    }

    for (int i = 0; i < count; i++) {
        Assignment a = assignments[start + i];
        int group_size = 0;
        for (int r = 0; r < num_requirements; r++) {
            if (requirements[r].id == a.class_req_id) {
                uint grp = requirements[r].group_id;
                if (grp < (uint)num_groups) {
                    group_size = group_sizes[grp];
                }
                break;
            }
        }
        if (a.room_id < (uint)num_rooms) {
            if (group_size > room_capacities[a.room_id]) {
                total_violations++;
            }
        }
    }

    fitness_out[gid] = 1.0f / (1.0f + (float)total_violations);
}
)";

        cl_int err;
        size_t source_len = strlen(kernel_source);
        program_ = clCreateProgramWithSource(context_, 1, &kernel_source, &source_len, &err);
        if (err != CL_SUCCESS) {
            throw std::runtime_error("Failed to create program");
        }

        err = clBuildProgram(program_, 1, &device_, "-cl-std=CL1.2", nullptr, nullptr);
        if (err != CL_SUCCESS) {
            // Get build log
            size_t log_size;
            clGetProgramBuildInfo(program_, device_, CL_PROGRAM_BUILD_LOG, 0, nullptr, &log_size);
            std::vector<char> log(log_size);
            clGetProgramBuildInfo(program_, device_, CL_PROGRAM_BUILD_LOG, log_size, log.data(), nullptr);
            std::cerr << "Build log:\n" << log.data() << "\n";
            throw std::runtime_error("Failed to build program");
        }

        kernel_ = clCreateKernel(program_, "evaluate_fitness", &err);
        if (err != CL_SUCCESS) {
            throw std::runtime_error("Failed to create kernel");
        }
    }

    void prepare_problem_data() {
        // Prepare requirements
        struct CLRequirement {
            cl_uint id;
            cl_uint subject_id;
            cl_uint group_id;
            cl_int hours_per_week;
        };
        std::vector<CLRequirement> reqs;
        for (const auto& r : problem_.requirements) {
            reqs.push_back({r.id, r.subject_id, r.group_id, r.hours_per_week});
        }

        // Prepare teacher subjects (flattened)
        std::vector<cl_uint> teacher_subjects;
        std::vector<cl_int> teacher_subject_offsets(problem_.teachers.size());
        std::vector<cl_int> teacher_subject_counts(problem_.teachers.size());

        for (size_t i = 0; i < problem_.teachers.size(); i++) {
            teacher_subject_offsets[i] = static_cast<cl_int>(teacher_subjects.size());
            teacher_subject_counts[i] = static_cast<cl_int>(problem_.teachers[i].subjects.size());
            for (Id s : problem_.teachers[i].subjects) {
                teacher_subjects.push_back(s);
            }
        }

        // Prepare room capacities
        std::vector<cl_int> room_capacities;
        for (const auto& r : problem_.rooms) {
            room_capacities.push_back(r.capacity);
        }

        // Prepare group sizes
        std::vector<cl_int> group_sizes;
        for (const auto& g : problem_.groups) {
            group_sizes.push_back(g.size);
        }

        // Create buffers
        cl_int err;

        requirements_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            reqs.size() * sizeof(CLRequirement), reqs.data(), &err);

        if (!teacher_subjects.empty()) {
            teacher_subjects_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                teacher_subjects.size() * sizeof(cl_uint), teacher_subjects.data(), &err);
        } else {
            cl_uint dummy = 0;
            teacher_subjects_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                sizeof(cl_uint), &dummy, &err);
        }

        teacher_subject_offsets_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            teacher_subject_offsets.size() * sizeof(cl_int), teacher_subject_offsets.data(), &err);

        teacher_subject_counts_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            teacher_subject_counts.size() * sizeof(cl_int), teacher_subject_counts.data(), &err);

        room_capacities_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            room_capacities.size() * sizeof(cl_int), room_capacities.data(), &err);

        group_sizes_buf_ = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            group_sizes.size() * sizeof(cl_int), group_sizes.data(), &err);

        num_requirements_ = static_cast<cl_int>(problem_.requirements.size());
        num_teachers_ = static_cast<cl_int>(problem_.teachers.size());
        num_rooms_ = static_cast<cl_int>(problem_.rooms.size());
        num_groups_ = static_cast<cl_int>(problem_.groups.size());
        total_timeslots_ = problem_.total_timeslots();
    }

    void evaluate_population_opencl(std::vector<Chromosome>& population) {
        // Flatten assignments
        struct CLAssignment {
            cl_uint class_req_id;
            cl_uint teacher_id;
            cl_uint room_id;
            cl_int timeslot_idx;
        };

        std::vector<CLAssignment> all_assignments;
        std::vector<cl_int> offsets(population.size());
        std::vector<cl_int> counts(population.size());

        for (size_t i = 0; i < population.size(); i++) {
            offsets[i] = static_cast<cl_int>(all_assignments.size());
            counts[i] = static_cast<cl_int>(population[i].assignments.size());
            for (const auto& a : population[i].assignments) {
                all_assignments.push_back({a.class_req_id, a.teacher_id, a.room_id, a.timeslot_idx});
            }
        }

        std::vector<cl_float> fitness_results(population.size());

        cl_int err;

        // Create buffers for this batch
        cl_mem assignments_buf = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            all_assignments.size() * sizeof(CLAssignment), all_assignments.data(), &err);

        cl_mem offsets_buf = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            offsets.size() * sizeof(cl_int), offsets.data(), &err);

        cl_mem counts_buf = clCreateBuffer(context_, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
            counts.size() * sizeof(cl_int), counts.data(), &err);

        cl_mem fitness_buf = clCreateBuffer(context_, CL_MEM_WRITE_ONLY,
            fitness_results.size() * sizeof(cl_float), nullptr, &err);

        // Set kernel arguments
        clSetKernelArg(kernel_, 0, sizeof(cl_mem), &assignments_buf);
        clSetKernelArg(kernel_, 1, sizeof(cl_mem), &offsets_buf);
        clSetKernelArg(kernel_, 2, sizeof(cl_mem), &counts_buf);
        clSetKernelArg(kernel_, 3, sizeof(cl_mem), &requirements_buf_);
        clSetKernelArg(kernel_, 4, sizeof(cl_int), &num_requirements_);
        clSetKernelArg(kernel_, 5, sizeof(cl_mem), &teacher_subjects_buf_);
        clSetKernelArg(kernel_, 6, sizeof(cl_mem), &teacher_subject_offsets_buf_);
        clSetKernelArg(kernel_, 7, sizeof(cl_mem), &teacher_subject_counts_buf_);
        clSetKernelArg(kernel_, 8, sizeof(cl_int), &num_teachers_);
        clSetKernelArg(kernel_, 9, sizeof(cl_mem), &room_capacities_buf_);
        clSetKernelArg(kernel_, 10, sizeof(cl_int), &num_rooms_);
        clSetKernelArg(kernel_, 11, sizeof(cl_mem), &group_sizes_buf_);
        clSetKernelArg(kernel_, 12, sizeof(cl_int), &num_groups_);
        clSetKernelArg(kernel_, 13, sizeof(cl_int), &total_timeslots_);
        clSetKernelArg(kernel_, 14, sizeof(cl_mem), &fitness_buf);

        // Execute kernel
        size_t global_size = population.size();
        err = clEnqueueNDRangeKernel(queue_, kernel_, 1, nullptr, &global_size, nullptr, 0, nullptr, nullptr);
        if (err != CL_SUCCESS) {
            std::cerr << "Kernel execution failed: " << err << "\n";
            // Fallback to CPU evaluation
            for (auto& c : population) {
                c.fitness = evaluator_.evaluate(c);
            }
        } else {
            // Read results
            clEnqueueReadBuffer(queue_, fitness_buf, CL_TRUE, 0,
                fitness_results.size() * sizeof(cl_float), fitness_results.data(), 0, nullptr, nullptr);

            for (size_t i = 0; i < population.size(); i++) {
                population[i].fitness = static_cast<double>(fitness_results[i]);
            }
        }

        // Cleanup batch buffers
        clReleaseMemObject(assignments_buf);
        clReleaseMemObject(offsets_buf);
        clReleaseMemObject(counts_buf);
        clReleaseMemObject(fitness_buf);
    }

    std::vector<Chromosome> evolve_generation(const std::vector<Chromosome>& population) {
        std::vector<Chromosome> next_gen;
        next_gen.reserve(ga_config_.population_size);

        std::mt19937 rng(std::random_device{}());
        std::uniform_real_distribution<double> prob(0.0, 1.0);

        while (static_cast<int>(next_gen.size()) < ga_config_.population_size) {
            const Chromosome& parent1 = operators_.tournament_select(population, ga_config_.tournament_size);
            const Chromosome& parent2 = operators_.tournament_select(population, ga_config_.tournament_size);

            Chromosome child1, child2;

            if (prob(rng) < ga_config_.crossover_rate) {
                auto [c1, c2] = operators_.crossover(parent1, parent2);
                child1 = std::move(c1);
                child2 = std::move(c2);
            } else {
                child1 = parent1;
                child2 = parent2;
            }

            operators_.mutate(child1, ga_config_.mutation_rate);
            operators_.mutate(child2, ga_config_.mutation_rate);

            next_gen.push_back(std::move(child1));
            if (static_cast<int>(next_gen.size()) < ga_config_.population_size) {
                next_gen.push_back(std::move(child2));
            }
        }

        return next_gen;
    }

    void cleanup_opencl() {
        if (kernel_) clReleaseKernel(kernel_);
        if (program_) clReleaseProgram(program_);
        if (requirements_buf_) clReleaseMemObject(requirements_buf_);
        if (teacher_subjects_buf_) clReleaseMemObject(teacher_subjects_buf_);
        if (teacher_subject_offsets_buf_) clReleaseMemObject(teacher_subject_offsets_buf_);
        if (teacher_subject_counts_buf_) clReleaseMemObject(teacher_subject_counts_buf_);
        if (room_capacities_buf_) clReleaseMemObject(room_capacities_buf_);
        if (group_sizes_buf_) clReleaseMemObject(group_sizes_buf_);
        if (queue_) clReleaseCommandQueue(queue_);
        if (context_) clReleaseContext(context_);
    }

    void sort_population(std::vector<Chromosome>& population) {
        std::sort(population.begin(), population.end());
    }

    double average_fitness(const std::vector<Chromosome>& population) const {
        double sum = 0;
        for (const auto& c : population) sum += c.fitness;
        return sum / population.size();
    }

    const ProblemConfig& problem_;
    GAConfig ga_config_;
    FitnessEvaluator evaluator_;
    GeneticOperators operators_;
    Chromosome best_ever_;
    double total_time_ms_ = 0;
    std::string device_name_;

    // OpenCL objects
    cl_device_id device_ = nullptr;
    cl_context context_ = nullptr;
    cl_command_queue queue_ = nullptr;
    cl_program program_ = nullptr;
    cl_kernel kernel_ = nullptr;

    // Problem data buffers
    cl_mem requirements_buf_ = nullptr;
    cl_mem teacher_subjects_buf_ = nullptr;
    cl_mem teacher_subject_offsets_buf_ = nullptr;
    cl_mem teacher_subject_counts_buf_ = nullptr;
    cl_mem room_capacities_buf_ = nullptr;
    cl_mem group_sizes_buf_ = nullptr;

    cl_int num_requirements_;
    cl_int num_teachers_;
    cl_int num_rooms_;
    cl_int num_groups_;
    cl_int total_timeslots_;
};

} // namespace timetable
