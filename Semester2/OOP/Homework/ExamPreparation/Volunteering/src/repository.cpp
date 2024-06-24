#include "repository.hpp"

#include <iostream>
#include <fstream>
#include <sstream>

Repository::Repository() {
    loadDepartments("../departments.txt");
    loadVolunteers("../volunteers.txt");
}

void Repository::loadDepartments(std::string filename) {
    std::ifstream file(filename);    

    std::string line;
    std::string name, description;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::getline(iss, name, '|');
        std::getline(iss, description, '|');

        departments.push_back(Department(name, description));
    }

    file.close();
}

void Repository::loadVolunteers(std::string filename) {
    std::ifstream file(filename);

    std::string line;
    std::string name, email, department, interestsString;
    std::vector<std::string> interests;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        std::getline(iss, name, '|');
        std::getline(iss, email, '|');
        std::getline(iss, interestsString, '|');
        std::getline(iss, department, '|');

        std::istringstream issInterests(interestsString);
        std::string interest;
        while (std::getline(issInterests, interest, ',')) {
            interests.push_back(interest);
        }

        volunteers.push_back(Volunteer(name, email, interests, department));
        interests.clear();
    }

    file.close();
}

void Repository::addVolunteer(Volunteer volunteer) {
    volunteers.push_back(volunteer);
}

void Repository::assignVolunteer(std::string name, std::string departmentName) {
    for (Volunteer &volunteer : volunteers) {
        if (volunteer.name == name) {
            volunteer.setDepartment(departmentName);
            return;
        }
    }
}

std::vector<Department> Repository::getDepartments() {
    return departments;
}

std::vector<Volunteer> Repository::getVolunteers() {
    return volunteers;
}