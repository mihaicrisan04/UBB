#include "subject.hpp"


void Subject::registerObserver(Observer *observer) {
    observers.emplace_back(observer);
}

void Subject::notify() {
    for (auto &observer : observers) {
        observer->update();
    }
}