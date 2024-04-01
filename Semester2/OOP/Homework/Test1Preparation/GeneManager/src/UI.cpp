#include "../headers/UI.h"
#include <iostream>


UI::UI(GeneService &service) : service{service} {}

void UI::printMenu() const {
    std::cout << "1. Add new gene\n";
    std::cout << "2. Show all genes\n";
    std::cout << "3. Show all genes with sequence\n";
    std::cout << "0. Exit\n";
}

void UI::addGene() {
    std::string name, organism, sequence;
    std::cout << "Enter the name: ";
    std::cin >> name;
    std::cout << "Enter the organism: ";
    std::cin >> organism;
    std::cout << "Enter the sequence: ";
    std::cin >> sequence;

    // validate input

    Gene gene{name, organism, sequence};

    try {
        service.addGene(gene);
    }
    catch (std::invalid_argument &e) {
        std::cout << e.what() << '\n';
    }
}

void UI::printGenes() {
    std::vector<Gene> genes = service.getGenes();
    for (const Gene &g : genes) {
        std::cout << g.toString() << '\n';
    }
}

void UI::printGenesWithSequence() {
    std::string seq;
    std::cout << "Enter the sequence: ";
    std::cin >> seq;

    // validate user input 
    std::vector<Gene> genes = service.getGenesWithSequence(seq);

    if (genes.size() == 0) {
        std::cout << "There are no genes with this sequence\n";
        return;
    }

    for (const Gene &g: genes) {
        std::cout << g.toString() << '\n';
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
        switch(option) {
            case 1:
                addGene();
                break;

            case 2:
                printGenes();
                break;

            case 3:
                printGenesWithSequence();
                break;
            
            default:
                std::cout << "Invalid option\n";
                break;
        }
    }
}