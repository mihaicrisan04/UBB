#pragma once
#include "Repository.h"
#include <string>


class GeneService {
private:
    GeneRepository &repo;

public:
    GeneService(GeneRepository &repo);

    void addGene(const Gene &gene);

    std::vector<Gene> getGenes() const;

    std::vector<Gene> getGenesWithSequence(std::string seq);
};