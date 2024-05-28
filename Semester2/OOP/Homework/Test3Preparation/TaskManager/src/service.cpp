#include "service.hpp"


Service::Service(std::string filename) : repo(filename) {}

std::vector<Task> Service::getTasks() {
    return repo.getTasks();
}