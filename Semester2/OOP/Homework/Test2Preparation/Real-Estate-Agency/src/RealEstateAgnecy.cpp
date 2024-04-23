#include "RealEstateAgency.h"
#include <algorithm>
#include <fstream>

RealEstateAgency::RealEstateAgency() : clients(), dwellings() {}

RealEstateAgency::~RealEstateAgency() {
    for (Client* c : clients) {
        delete c;
    }
}

void RealEstateAgency::addClient(Client* c) {
    clients.push_back(c);
}

void RealEstateAgency::addDwelling(Dwelling d) {
    dwellings.push_back(d);
}

void RealEstateAgency::removeClient(std::string name) {
    clients.erase(std::remove_if(clients.begin(), clients.end(), 
    [name](const Client* c) { return c->getName() == name; }), clients.end());
}

std::vector<Client*> RealEstateAgency::getInterestedClients(Dwelling d) {
    std::vector<Client*> interestedClients;
    for (Client* c : clients) {
        if (c->isInterested(d)) {
            interestedClients.push_back(c);
        }
    }
    return interestedClients;
}

void RealEstateAgency::writeToFile(std::string filename) {
    std::ofstream file(filename);
    for (Client* c : clients) {
        file << c->toString() << std::endl;
    }
    file.close();   
}