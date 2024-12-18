#pragma once

#include <memory>
#include <vector>

#include "observer.hpp"

class Subject {
private:
    std::vector<std::unique_ptr<Observer>> observers;

public:
    void addObserver(Observer* observer) {
        observers.emplace_back(observer);
    }

    void notify() {
        for (auto& observer : observers) {
            observer->update();
        }
    }
};
