#pragma once

#include "types.hpp"
#include <unordered_map>
#include <algorithm>

namespace timetable {

class FitnessEvaluator {
public:
    explicit FitnessEvaluator(const ProblemConfig& config) : config_(config) {
        // Build lookup maps for faster evaluation
        for (const auto& req : config_.requirements) {
            req_to_subject_[req.id] = req.subject_id;
            req_to_group_[req.id] = req.group_id;
        }
        for (const auto& teacher : config_.teachers) {
            for (Id subj : teacher.subjects) {
                teacher_subjects_[teacher.id].insert(subj);
            }
        }
        for (const auto& room : config_.rooms) {
            room_capacity_[room.id] = room.capacity;
        }
        for (const auto& group : config_.groups) {
            group_size_[group.id] = group.size;
        }
    }

    // Evaluate fitness of a chromosome (higher is better, 1.0 is perfect)
    double evaluate(const Chromosome& chromosome) const {
        int total_violations = 0;

        // Count conflicts per timeslot
        std::unordered_map<int, std::vector<const Assignment*>> by_timeslot;
        for (const auto& a : chromosome.assignments) {
            by_timeslot[a.timeslot_idx].push_back(&a);
        }

        for (const auto& [slot, assignments] : by_timeslot) {
            total_violations += count_teacher_conflicts(assignments);
            total_violations += count_room_conflicts(assignments);
            total_violations += count_group_conflicts(assignments);
        }

        // Check teacher-subject compatibility
        total_violations += count_teacher_subject_violations(chromosome);

        // Check room capacity violations
        total_violations += count_capacity_violations(chromosome);

        // Fitness: 1 / (1 + violations)
        return 1.0 / (1.0 + total_violations);
    }

private:
    // Count teachers assigned to multiple classes at same time
    int count_teacher_conflicts(const std::vector<const Assignment*>& assignments) const {
        std::unordered_map<Id, int> teacher_count;
        for (const auto* a : assignments) {
            teacher_count[a->teacher_id]++;
        }
        int conflicts = 0;
        for (const auto& [tid, count] : teacher_count) {
            if (count > 1) conflicts += count - 1;
        }
        return conflicts;
    }

    // Count rooms used by multiple classes at same time
    int count_room_conflicts(const std::vector<const Assignment*>& assignments) const {
        std::unordered_map<Id, int> room_count;
        for (const auto* a : assignments) {
            room_count[a->room_id]++;
        }
        int conflicts = 0;
        for (const auto& [rid, count] : room_count) {
            if (count > 1) conflicts += count - 1;
        }
        return conflicts;
    }

    // Count student groups with multiple classes at same time
    int count_group_conflicts(const std::vector<const Assignment*>& assignments) const {
        std::unordered_map<Id, int> group_count;
        for (const auto* a : assignments) {
            auto it = req_to_group_.find(a->class_req_id);
            if (it != req_to_group_.end()) {
                group_count[it->second]++;
            }
        }
        int conflicts = 0;
        for (const auto& [gid, count] : group_count) {
            if (count > 1) conflicts += count - 1;
        }
        return conflicts;
    }

    // Count teachers assigned to subjects they can't teach
    int count_teacher_subject_violations(const Chromosome& chromosome) const {
        int violations = 0;
        for (const auto& a : chromosome.assignments) {
            auto req_it = req_to_subject_.find(a.class_req_id);
            if (req_it == req_to_subject_.end()) continue;

            Id subject = req_it->second;
            auto teacher_it = teacher_subjects_.find(a.teacher_id);
            if (teacher_it == teacher_subjects_.end() ||
                teacher_it->second.find(subject) == teacher_it->second.end()) {
                violations++;
            }
        }
        return violations;
    }

    // Count room capacity violations
    int count_capacity_violations(const Chromosome& chromosome) const {
        int violations = 0;
        for (const auto& a : chromosome.assignments) {
            auto req_it = req_to_group_.find(a.class_req_id);
            auto cap_it = room_capacity_.find(a.room_id);
            if (req_it == req_to_group_.end() || cap_it == room_capacity_.end()) continue;

            auto size_it = group_size_.find(req_it->second);
            if (size_it != group_size_.end() && size_it->second > cap_it->second) {
                violations++;
            }
        }
        return violations;
    }

    const ProblemConfig& config_;
    std::unordered_map<Id, Id> req_to_subject_;
    std::unordered_map<Id, Id> req_to_group_;
    std::unordered_map<Id, std::set<Id>> teacher_subjects_;
    std::unordered_map<Id, int> room_capacity_;
    std::unordered_map<Id, int> group_size_;
};

} // namespace timetable
