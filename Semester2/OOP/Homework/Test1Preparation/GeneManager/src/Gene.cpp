#include "../headers/Gene.h"

Gene::Gene(const std::string &name, const std::string &organism, const std::string sequence) : 
    name{name}, 
    organism{organism}, 
    sequence{sequence} {}

std::string Gene::getName() const {
    return name;
}

std::string Gene::getOrganism() const {
    return organism;
}

std::string Gene::getSequence() const {
    return sequence;
}

std::string Gene::toString() const {
    return name + " " + organism + " " + sequence; 
}

bool Gene::operator==(const Gene &other) const {
    return name == other.name && organism == other.organism;
}

bool Gene::operator!=(const Gene &other) const {
    return !(*this == other);
}

Gene& Gene::operator=(const Gene &other) {
    if (*this != other) {
        name = other.name;
        organism = other.organism;
        sequence = other.sequence;
    }
    return *this;
}