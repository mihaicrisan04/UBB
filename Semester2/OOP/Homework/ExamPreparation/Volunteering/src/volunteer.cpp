#include "volunteer.hpp"


Volunteer::Volunteer(std::string name, std::string email, std::vector<std::string> interests, std::string department) : name(name), email(email), interests(interests), department(department) {}

void Volunteer::setDepartment(std::string department) {
    this->department = department;
}