#include "repository.hpp"

#include <fstream>
#include <string.h>

Repository::Repository(std::string filename) {
    readData(filename);
}


void Repository::readData(std::string filename) {
    std::ifstream fin(filename);

    char buffer[100];
    while (fin.getline(buffer, 99)) {
        std::string description = strtok(buffer, ",");
        int duration = atoi(strtok(NULL, ","));
        int priority = atoi(strtok(NULL, ","));

        addTask(Task(description, duration, priority));
    }

    fin.close();
}

void Repository::addTask(Task t) {
    tasks.push_back(t);
}

std::vector<Task> Repository::getTasks() {
    return tasks;
}