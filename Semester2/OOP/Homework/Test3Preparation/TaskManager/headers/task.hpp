#pragma once

#include <string>


class Task {
    public:
        std::string description;
        int duration;
        int priority;

        Task(std::string description, int duration, int priority);
        ~Task() = default;
};