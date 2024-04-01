#pragma once
#include <string>

class Gene {
private:
    std::string name;
    std::string organism;
    std::string sequence;

public:
    Gene(const std::string &name, const std::string &organism, const std::string sequence);

    std::string getName() const;

    std::string getOrganism() const;

    std::string getSequence() const;

    std::string toString() const;

    bool operator==(const Gene &other) const;

    bool operator!=(const Gene &other) const;

    Gene& operator=(const Gene &other);
};