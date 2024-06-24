#pragma once

#include <vector>
#include <memory>

#include "observer.hpp"


class Subject {
    private:
        std::vector<std::unique_ptr<Observer>> observers;

    public:
        void registerObserver(Observer *observer);
        void notify();
};
