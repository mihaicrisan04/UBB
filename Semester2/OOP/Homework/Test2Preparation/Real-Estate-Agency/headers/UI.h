#pragma once
#include "RealEstateAgency.h"
#include "Dwelling.h"


class UI {
    private:
        RealEstateAgency* agency;

    public:
        UI(RealEstateAgency* agency);

        void printMenu();

        void run();

        void addClient();

        void addDwelling();

        void removeClient();

        void getInterestedClients();

        void writeToFile();

        ~UI();
};