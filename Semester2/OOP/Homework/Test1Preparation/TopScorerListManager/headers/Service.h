#include "Repository.h"


class PlayerService {
private:
    PlayerRepository &repo;

public:
    PlayerService(PlayerRepository &repo);

    void addPlayer(const Player &player);

    void removePlayer(const Player &player);

    std::vector<Player> getPlayers() const;

    std::vector<Player> getPlayersWithNationality(std::string nationality);
};