#pragma once

#include <vector>
#include <string>

#include "department.hpp"
#include "volunteer.hpp"


class Repository {
    private:
        std::vector<Department> departments;
        std::vector<Volunteer> volunteers;

    public:
        Repository();
        ~Repository() = default;

        void loadDepartments(std::string filename);
        void loadVolunteers(std::string filename);

        void addVolunteer(Volunteer volunteer);
        void assignVolunteer(std::string name, std::string departmentName);

        std::vector<Department> getDepartments();
        std::vector<Volunteer> getVolunteers();
};