#include "../headers/tests.h"
#include "../headers/Repository.h"
#include "../headers/Service.h"
#include <iostream>


void testAll() {
    testAddPlayer();
    testShowPlayersWithNationality();
    std::cout << "All tests passed\n";
}

void testAddPlayer() {
    // repo already contains 7 players. start from size 7
    std::cout << "Testing addPlayer\n";
    PlayerRepository repo;
    Player player1{"Cristiano", "Portugal", "Juventus", 30};
    Player player2{"Lionel", "Argentina", "Barcelona", 25};
    Player player3{"Robert", "Poland", "Bayern", 35};

    repo.addPlayer(player1);
    repo.addPlayer(player2);
    repo.addPlayer(player3);

    assert(repo.getPlayers().size() == 7 + 3);

    try {
        repo.addPlayer(player1);
        assert(false);
    }
    catch (std::invalid_argument &e) {
        assert(true);
    }
}

void testShowPlayersWithNationality() {
    std::cout << "Testing getPlayersWithNationality\n";

    // repo already contains 7 players, 2 of them are from ROU
    // testing for players from ROU
    PlayerRepository repo;
    PlayerService service{repo};
    
    std::vector<Player> players = service.getPlayersWithNationality("ROU");

    assert(players.size() == 2);
}