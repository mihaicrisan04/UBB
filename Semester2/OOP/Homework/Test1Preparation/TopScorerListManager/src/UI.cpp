#include "../headers/UI.h"
#include <stdexcept>
#include <iostream>


UI::UI(PlayerService &service) : service{service} {}

void UI::printMenu() const {
    std::cout << "1. Add player\n";
    std::cout << "2. Remove player\n";
    std::cout << "3. Show players\n";
    std::cout << "4. Show Players by Nationality\n";
    std::cout << "0. Exit\n";
}

void UI::addPlayer() {

}

void UI::RemovePlayer() {
    std::string name;
    std::cout << "Enter the name: ";
    std::cin >> name;

    Player p("name", "", "", 0);

    try {
        service.removePlayer(p);
        std::cout << "Player removed succesfuly\n";
    }
    catch (std::invalid_argument &e) {
        std::cout << e.what() << '\n';
    }

}

void UI::ShowPlayers() {
    std::vector<Player> players = service.getPlayers();

    for (const Player &p : players) {
        std::cout << p.toString() << '\n';
    }
}

void UI::ShowPlayersByNationality() {
    std::string nationality;
    std::cout << "Enter the nationality: ";
    std::cin >> nationality;

    std::vector<Player> players = service.getPlayersWithNationality(nationality);

    if (players.size() == 0) {
        std::cout << "There are no players form this country\n";
        return;
    }

    for (const Player &p : players) {
        std::cout << p.toString() << '\n';
    }
}

void UI::run() {
    while (true) {
        printMenu();
        int option;
        std::cout << ">>>";
        std::cin >> option;

        if (option == 0) {
            break;
        }
        switch (option) {
        case 1:
            addPlayer();
            break;

        case 2:
            RemovePlayer();
            break;

        case 3:
            ShowPlayers();
            break;
        
        case 4:
            ShowPlayersByNationality();
            break;

        default:
            std::cout << "Invalid option\n";
            break;
        }
    }
}