#pragma once

#include "model.hpp"    

#include <vector>   
#include <fstream>
#include <sstream>

class Repository {
private:
    std::vector<Courier> couriers;
    std::vector<Package> packages;  

public:
    Repository() {
        loadCouriers();
        loadPackages();
    }

    ~Repository() {
        saveCouriers();
        savePackages();
    }   

    void addCourier(Courier courier) {
        couriers.push_back(courier);
    }

    void addPackage(Package package) {
        packages.push_back(package);
    }

    std::vector<Courier> getCouriers() {
        return couriers;
    }

    std::vector<Package> getPackages() {
        return packages;
    }

    void loadCouriers() {
        std::ifstream file("../couriers.txt");
        std::string line;

        while (std::getline(file, line)) {
            std::stringstream ss(line);
            std::string name;
            std::string streetsStr;
            std::string zoneStr;
            std::getline(ss, name, '|');
            std::getline(ss, streetsStr, '|');
            std::getline(ss, zoneStr, '|');

            std::vector<std::string> streets;
            std::stringstream streetsStream(streetsStr);
            std::string street;
            while (std::getline(streetsStream, street, ',')) {
                streets.push_back(street);
            }

            std::stringstream zoneStream(zoneStr);
            int x, y, r;
            std::string token;
            std::getline(zoneStream, token, ',');
            x = std::stoi(token);
            std::getline(zoneStream, token, ',');
            y = std::stoi(token);
            std::getline(zoneStream, token, ',');
            r = std::stoi(token);

            Courier courier(name, streets, {x, y, r});
            addCourier(courier);
        }

        file.close();
    }

    void loadPackages() {
        std::ifstream file("../packages.txt");
        std::string line;

        while (std::getline(file, line)) {
            std::stringstream ss(line);
            std::string recipient;
            std::string address;
            std::string locationStr;
            std::string deliveredStr;
            std::getline(ss, recipient, '|');
            std::getline(ss, address, '|');
            std::getline(ss, locationStr, '|');
            std::getline(ss, deliveredStr, '|');

            std::stringstream locationStream(locationStr);
            int x, y;
            std::string token;
            std::getline(locationStream, token, ',');
            x = std::stoi(token);
            std::getline(locationStream, token, ',');
            y = std::stoi(token);

            bool delivered = deliveredStr == "true" ? true : false;

            Package package(recipient, address, {x, y}, delivered);
            addPackage(package);
        }

        file.close();
    }

    void saveCouriers() {
        std::ofstream file("../couriers.txt");  

        for (auto courier : couriers) {
            file << courier.name << "|";
            for (auto street : courier.streets) {
                file << street << ",";
            }
            file << "|";
            file << std::get<0>(courier.zone) << "," << std::get<1>(courier.zone) << "," << std::get<2>(courier.zone) << "\n";
        }

        file.close();
    }

    void savePackages() {
        std::ofstream file("../packages.txt");

        for (auto package : packages) {
            file << package.recipient << "|";
            file << package.address << "|";
            file << std::get<0>(package.location) << "," << std::get<1>(package.location) << "|";
            file << (package.delivered ? "true" : "false") << "\n";
        }

        file.close();
    }
};