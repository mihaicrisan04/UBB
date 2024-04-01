#include "../headers/Repository.h"
#include <stdexcept>


GeneRepository::GeneRepository() : genes{} {
    genes.push_back(Gene("Gene1", "Organism1", "Sequence1"));
    genes.push_back(Gene("Gene2", "Organism2", "Sequence2"));
    genes.push_back(Gene("Gene3", "Organism3", "Sequence3"));
}

void GeneRepository::addGene(const Gene &gene) {
    for (const Gene &g : genes) {
        if (g == gene) {
            throw std::invalid_argument("Gene already exists!");
        }
    }
    genes.push_back(gene);
}

std::vector<Gene> GeneRepository::getGenes() const {
    return genes;
}