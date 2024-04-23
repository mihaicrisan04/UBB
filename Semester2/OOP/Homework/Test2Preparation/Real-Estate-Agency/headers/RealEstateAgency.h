#pragma once
#include <vector>
#include "NormalClient.h"
#include "WealthyClient.h"
#include "Dwelling.h"


class RealEstateAgency {
    private:
        std::vector<Client*> clients;
        std::vector<Dwelling> dwellings;

    public:
        RealEstateAgency();

        void addClient(Client* c);

        void addDwelling(Dwelling d);

        void removeClient(std::string name);

        std::vector<Client*> getInterestedClients(Dwelling d);

        void writeToFile(std::string filename);

        ~RealEstateAgency();
};