#include "../headers/Service.h"
#include <stdexcept>
#include <algorithm>


PlayerService::PlayerService(PlayerRepository &repo) : repo{repo} {}

void PlayerService::addPlayer(const Player &player) {
    try {
        repo.addPlayer(player);
    }
    catch (std::invalid_argument &e) {
        throw e;
    }
}

void PlayerService::removePlayer(const Player &player) {
    try {
        repo.removePlayer(player);
    }
    catch (std::invalid_argument &e) {
        throw e;
    }
}

std::vector<Player> PlayerService::getPlayers() const {
    return repo.getPlayers();
}

std::vector<Player> PlayerService::getPlayersWithNationality(std::string nationality) {
    std::vector<Player> players;

    for (const Player &p : repo.getPlayers()) {
        if (p.getNationality() == nationality) {
            players.push_back(p);
        }
    }   

    sort(players.begin(), players.end(), [](const Player &a, const Player &b) {
        return a.getGoals() < b.getGoals();
    });

    return players;
}