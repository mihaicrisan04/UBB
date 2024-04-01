#include "../headers/Service.h"
#include <stdexcept>
#include <algorithm>


GeneService::GeneService(GeneRepository &repo) : repo{repo} {}

void GeneService::addGene(const Gene &gene) {
    try {
        repo.addGene(gene);
    }
    catch (std::invalid_argument &e) {
        throw;
    }
}

std::vector<Gene> GeneService::getGenes() const {
    std::vector<Gene> genes = repo.getGenes();
    sort(genes.begin(), genes.end(), [](const Gene &a, const Gene &b) {
        return a.getOrganism() < b.getOrganism();
    });
    return genes;
}

std::vector<Gene> GeneService::getGenesWithSequence(std::string seq) {
    std::vector<Gene> genes;
    for (const Gene &g : repo.getGenes()) {
        if (g.getSequence().find(seq) != std::string::npos) {
            genes.push_back(g);
        }
    }
    return genes;
}
