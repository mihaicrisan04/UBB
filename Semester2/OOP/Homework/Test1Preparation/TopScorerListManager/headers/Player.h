#pragma once
#include <string>


class Player {
private:
    std::string name;
    std::string nationality;
    std::string team;
    int goals;

public:
    Player(const std::string &name, const std::string &nationality, const std::string &team, int goals);

    std::string getName() const;

    std::string getNationality() const;

    std::string getTeam() const;

    int getGoals() const;

    std::string toString() const;

    bool operator==(const Player &other) const;    

    bool operator!=(const Player &other) const;

    Player& operator=(const Player &other);
};