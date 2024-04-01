#pragma once
#include "Service.h"


class UI {
private:
    GeneService &service;

public:
    UI(GeneService &service);

    void printMenu() const;

    void addGene();

    void printGenes();

    void printGenesWithSequence();

    void run();
};