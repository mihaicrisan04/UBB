#pragma once

#include "observer.hpp"

#include <vector>
#include <memory>


class Subject {
    private:
        std::vector<std::unique_ptr<Observer>> observers;

    public:
        Subject() = default;
        ~Subject() = default;

        void addObserver(Observer *observer) {
            observers.emplace_back(observer);
        }

        void notify() {
            for (auto &observer: observers) {
                observer->update();
            }
        }
};