#pragma once

#include <string>
#include <vector>
#include <tuple>


class Courier {
public:
    std::string name;
    std::vector<std::string> streets;
    std::tuple<int, int, int> zone;

    Courier(std::string name, std::vector<std::string> streets, std::tuple<int, int, int> zone) : name(name), streets(streets), zone(zone) {}
};

class Package {
public:
    std::string recipient;
    std::string address;
    std::tuple<int, int> location;
    bool delivered;

    Package(std::string recipient, std::string address, std::tuple<int, int> location, bool delivered = false) : recipient(recipient), address(address), location(location), delivered(delivered) {}

};