#pragma once
#include "Gene.h"
#include <vector>


class GeneRepository {
private:
    std::vector<Gene> genes;

public:
    GeneRepository();

    std::vector<Gene> getGenes() const;

    void addGene(const Gene &gene);

};