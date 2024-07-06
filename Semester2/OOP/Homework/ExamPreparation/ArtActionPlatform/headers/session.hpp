#pragma once

#include "repository.hpp"
#include "subject.hpp"


class Session: public Subject {
private:
    Repository &repo;

public:
    Session(Repository &repo): repo(repo) {}

    void addItem(Item item) {
        repo.addItem(item);
        notify();
    }

    void addUser(User user) {
        repo.addUser(user);
        notify();
    }

    std::vector<Item> getItems() {
        return repo.getItems();
    }

    std::vector<User> getUsers() {
        return repo.getUsers();
    }
};