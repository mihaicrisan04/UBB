
#include "service.hpp"


Service::Service(Repository &repo) : repo{repo} {}

std::vector<Department> Service::getDepartments() {
    return repo.getDepartments();
}

std::vector<Volunteer> Service::getVolunteers() {
    return repo.getVolunteers();
}

void Service::addVolunteer(Volunteer volunteer) {
    repo.addVolunteer(volunteer);
    notify();
}

void Service::assignVolunteer(std::string name, std::string departmentName) {
    for (Volunteer &volunteer : repo.getVolunteers()) {
        if (volunteer.name == name) {
            repo.assignVolunteer(name, departmentName);
            notify();
            return;
        }
    }
}

std::vector<Volunteer> Service::getMostSuitableVolunteers(std::string departmentName) {
    std::vector<Volunteer> volunteers = repo.getVolunteers();
    std::vector<Volunteer> suitableVolunteers;

    for (Volunteer volunteer : volunteers) {
        if (volunteer.department == "") {
            for (std::string interest : volunteer.interests) {
                if (interest == departmentName) {
                    suitableVolunteers.push_back(volunteer);
                    break;
                }
            }
        }
    }

    return suitableVolunteers;
}

