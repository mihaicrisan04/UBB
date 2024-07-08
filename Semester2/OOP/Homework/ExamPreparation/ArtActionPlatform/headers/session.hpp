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

    std::vector<std::string> getCategories() {
        std::vector<std::string> categories;
        for (auto item: repo.getItems()) {
            bool found = false;
            for (auto category: categories) {
                if (category == item.category) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                categories.push_back(item.category);
            }
        }
        return categories;
    }

    std::vector<Item> getItems() {
        return repo.getItems();
    }

    std::vector<User> getUsers() {
        return repo.getUsers();
    }
};