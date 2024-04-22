#include "Controller.h"
#include <algorithm>


Controller::Controller() : departments() {}

Controller::~Controller() {
    for (auto &deparment : departments) {
        delete deparment;
    }
}

void Controller::addDepartment(HospitalDepartment *department) {
    departments.push_back(department);
}

std::vector<HospitalDepartment *> Controller::getAllDepartments() const {
    return departments;
}

std::vector<HospitalDepartment *> Controller::getAllEfficientDepartments() const {
    std::vector<HospitalDepartment *> result;
    for (auto &department : departments) {
        if (department->isEfficient()) {
            result.push_back(department);
        }
    }
    return result;
}

void Controller::writeToFile(std::string filename) const {
    std::ofstream file(filename);

    std::vector<HospitalDepartment *> departmentsCopy = departments;
    sort(departmentsCopy.begin(), departmentsCopy.end(), [](HospitalDepartment *a, HospitalDepartment *b) {
        return *a < *b;
    });

    for (auto &department : departmentsCopy) {
        file << department->toString() << '\n';
    }

    file.close();
}