#include "../headers/Player.h"
#include <string>

Player::Player(const std::string &name, const std::string &nationality, const std::string &team, int goals) : name{name}, nationality{nationality}, team{team}, goals{goals} {}

std::string Player::getName() const {
    return name;
}

std::string Player::getNationality() const {
    return nationality;
}

std::string Player::getTeam() const {
    return team;
}

int Player::getGoals() const {
    return goals;
}

std::string Player::toString() const {
    return name + " " + nationality + " " + team + " " + std::to_string(goals);
}

bool Player::operator==(const Player &other) const {
    return name == other.name;
}

bool Player::operator!=(const Player &other) const {
    return !(*this == other);
}

Player& Player::operator=(const Player &other) {
    if (*this != other) {
        name = other.name;
        nationality = other.nationality;
        team = other.team;
        goals = other.goals; 
    }
    return *this;
}

