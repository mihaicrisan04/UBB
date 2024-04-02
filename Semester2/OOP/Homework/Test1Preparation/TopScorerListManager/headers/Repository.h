#pragma once
#include "Player.h"
#include <vector>


class PlayerRepository {
private:
    std::vector<Player> players;

public:
    PlayerRepository();

    std::vector<Player> getPlayers() const;

    void addPlayer(const Player &player);

    void removePlayer(const Player &player); 
};