#pragma once

#include <string>

class Idea {
    public:
        std::string title;
        std::string description;
        std::string status;
        std::string creator;
        int duration;

        Idea(std::string title, std::string description, std::string status, std::string creator, int duration) : title(title), description(description), status(status), creator(creator), duration(duration) {}

        std::string toString() {
            return title + "," + description + "," + status + "," + creator + "," + std::to_string(duration);
        }
};
