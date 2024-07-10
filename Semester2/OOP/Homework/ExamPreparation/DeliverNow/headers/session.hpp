#pragma once

#include "subject.hpp"
#include "repository.hpp"


class Session : public Subject {
private:
    Repository& repo;

public:
    Session(Repository& repo) : repo{repo} {}


    void addCourier(Courier courier) {
        repo.addCourier(courier);
        notify();
    }

    void addPackage(Package package) {
        repo.addPackage(package);
        notify();
    }

    std::vector<Courier> getCouriers() {
        return repo.getCouriers();
    }

    std::vector<Package> getPackages() {
        return repo.getPackages();
    }
};