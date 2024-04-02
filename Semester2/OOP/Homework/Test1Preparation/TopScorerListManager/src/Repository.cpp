#include "../headers/Repository.h"
#include <stdexcept>
#include <algorithm>

PlayerRepository::PlayerRepository() : players{} {
    players.push_back(Player("Nora_Mork", "NOR", "Larvik", 83));
    players.push_back(Player("Isabelle_Gullden", "SWE", "CSM_Bucurest", 80));
    players.push_back(Player("Cristina_Neagu", "ROU", "Buducnost", 63));
    players.push_back(Player("Allison_Pineau", "FRA", "HCM_Baia_Mare", 82));
    players.push_back(Player("Ilina_Ekaterina", "RUS", "Rostov-Don", 80));
    players.push_back(Player("Nerea_Pena", "ESP", "FTC-Rail_Cargo_Hungaria", 59));
    players.push_back(Player("Hatz", "ROU", "Larvik", 83));
}

std::vector<Player> PlayerRepository::getPlayers() const {
    return players;
}

void PlayerRepository::addPlayer(const Player &player) {
    for (const Player &p : players) {
        if (p == player) {
            throw std::invalid_argument("Playe already exists");
        }
    }
    players.push_back(player);
} 

void PlayerRepository::removePlayer(const Player &player) {
    bool exists = false;
    for (const Player &p : players) {
        if (p == player) {
            exists = true;
        }
    }

    if (!exists) {
        throw std::invalid_argument("No player with that name");
    } 

    players.erase(std::remove(players.begin(), players.end(), player), players.end());
}
