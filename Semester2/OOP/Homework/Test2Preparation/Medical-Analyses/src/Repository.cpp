#include "Repository.h" 
#include <fstream>


Repository::Repository() = default;

void Repository::addAnalysis(MedicalAnalysis *analysis) {
    medicalAnalyses.push_back(analysis);
}

std::vector<MedicalAnalysis *> Repository::getAllAnalyses() {
    return medicalAnalyses;
}

std::vector<MedicalAnalysis *> Repository::getAnalysesByMonth(int month) {
    std::vector<MedicalAnalysis *> analyses;
    for (auto &analysis : medicalAnalyses) {
        if (std::stoi(analysis->getDate().substr(5, 2)) == month) {
            analyses.push_back(analysis);
        }
    }
    return analyses;
}

void Repository::writeToFile(std::string filename) {
    std::ofstream file(filename);
    for (auto &analysis : medicalAnalyses) {
        file << analysis->toString() << '\n';
    }
    file.close();
}
