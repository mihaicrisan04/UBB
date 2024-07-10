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

    void updatePackageDeliveredStatusOf(std::string recipientName) {
        repo.updatePackageDeliveredStatusOf(recipientName);
        notify();
    }

    std::vector<Courier> getCouriers() {
        return repo.getCouriers();
    }

    std::vector<Package> getPackages() {
        return repo.getPackages();
    }

    std::vector<std::string> getStreetsOf(Courier courier) {
        std::vector<std::string> streets;

        for (auto package: getPackagesOf(courier)) {
            if (package.delivered) { continue; }
            streets.push_back(package.address);
        }

        return streets;
    }

    std::vector<Package> getPackagesOf(Courier courier) {
        std::vector<Package> packages;

        for (auto package: getPackages()) {
            if (package.delivered) { continue; }

            int x = std::get<0>(package.location);
            int y = std::get<1>(package.location);

            int x1 = std::get<0>(courier.zone);
            int y1 = std::get<1>(courier.zone);
            int r = std::get<2>(courier.zone);

            if ((x - x1) * (x - x1) + (y - y1) * (y - y1) <= r * r) {
                packages.push_back(package);
                continue;
            }

            if (std::find(courier.streets.begin(), courier.streets.end(), package.address) != courier.streets.end()) {
                packages.push_back(package);
                continue;
            }
        }

        return packages;
    }
};