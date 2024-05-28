#pragma once

#include "repository.hpp"


class Service {
    private: 
        Repository repo;
    
    public:
        Service(std::string filename);
        ~Service() = default;

        std::vector<Task> getTasks();
};