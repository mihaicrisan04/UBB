// OpenCL Kernels for Timetable GA Fitness Evaluation

// Structure definitions (must match C++ side)
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

// Fitness evaluation kernel
// Each work item evaluates one chromosome
__kernel void evaluate_fitness(
    __global const Assignment* assignments,      // All assignments (flattened)
    __global const int* assignment_offsets,      // Start index for each chromosome
    __global const int* assignment_counts,       // Number of assignments per chromosome
    __global const ClassRequirement* requirements,
    const int num_requirements,
    __global const uint* teacher_subjects,       // Flattened: [teacher0_subj0, teacher0_subj1, ...]
    __global const int* teacher_subject_offsets, // Start index for each teacher
    __global const int* teacher_subject_counts,  // Count for each teacher
    const int num_teachers,
    __global const int* room_capacities,
    const int num_rooms,
    __global const int* group_sizes,
    const int num_groups,
    const int total_timeslots,
    __global double* fitness_out)
{
    int gid = get_global_id(0);  // Chromosome index

    int start = assignment_offsets[gid];
    int count = assignment_counts[gid];

    int total_violations = 0;

    // For each timeslot, count conflicts
    for (int slot = 0; slot < total_timeslots; slot++) {
        // Count teachers, rooms, and groups used in this slot
        // Using local arrays (limited size for embedded use)
        int teacher_count[64] = {0};  // Assuming max 64 teachers
        int room_count[64] = {0};     // Assuming max 64 rooms
        int group_count[64] = {0};    // Assuming max 64 groups

        for (int i = 0; i < count; i++) {
            Assignment a = assignments[start + i];
            if (a.timeslot_idx == slot) {
                if (a.teacher_id < 64) teacher_count[a.teacher_id]++;
                if (a.room_id < 64) room_count[a.room_id]++;

                // Find group for this class requirement
                for (int r = 0; r < num_requirements; r++) {
                    if (requirements[r].id == a.class_req_id) {
                        uint grp = requirements[r].group_id;
                        if (grp < 64) group_count[grp]++;
                        break;
                    }
                }
            }
        }

        // Count violations
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

    // Check teacher-subject compatibility
    for (int i = 0; i < count; i++) {
        Assignment a = assignments[start + i];

        // Find subject for this requirement
        uint subject = 0;
        for (int r = 0; r < num_requirements; r++) {
            if (requirements[r].id == a.class_req_id) {
                subject = requirements[r].subject_id;
                break;
            }
        }

        // Check if teacher can teach this subject
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

    // Check room capacity
    for (int i = 0; i < count; i++) {
        Assignment a = assignments[start + i];

        // Find group and its size
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

        // Check room capacity
        if (a.room_id < (uint)num_rooms) {
            if (group_size > room_capacities[a.room_id]) {
                total_violations++;
            }
        }
    }

    // Fitness: 1 / (1 + violations)
    fitness_out[gid] = 1.0 / (1.0 + (double)total_violations);
}
