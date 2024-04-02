#include "Service.h"


class UI {
private:
    PlayerService &service;

public:
    UI(PlayerService &service);

    void printMenu() const;

    void addPlayer();

    void RemovePlayer();

    void ShowPlayers();

    void ShowPlayersByNationality();

    void run();
};