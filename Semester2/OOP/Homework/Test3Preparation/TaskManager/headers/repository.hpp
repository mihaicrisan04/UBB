#pragma once

#include "task.hpp"
#include <vector>


class Repository {
    private:
        std::vector<Task> tasks;

    public:
        Repository(std::string filename);
        ~Repository() = default;

        void addTask(Task t);
        std::vector<Task> getTasks();
        void readData(std::string filename);
};