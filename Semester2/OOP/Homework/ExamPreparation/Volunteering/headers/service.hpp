#pragma once

#include "repository.hpp"
#include "subject.hpp"


class Service: public Subject {
    private: 
        Repository &repo;

    public:
        Service(Repository &repo);
        ~Service() = default;

        void addVolunteer(Volunteer volunteer);
        void assignVolunteer(std::string name, std::string departmentName);

        std::vector<Volunteer> getMostSuitableVolunteers(std::string departmentName);

        std::vector<Department> getDepartments();
        std::vector<Volunteer> getVolunteers();
};