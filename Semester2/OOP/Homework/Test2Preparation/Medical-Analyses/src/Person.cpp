#include "Person.h"
#include <fstream>
#include <string>


Person::Person(std::string &name) : name(name) {}

void Person::addAnalysis(MedicalAnalysis *analysis) {
    repository.addAnalysis(analysis);
}

std::vector<MedicalAnalysis *> Person::getAllAnalyses() {
    return repository.getAllAnalyses();
}

std::vector<MedicalAnalysis *> Person::getAnalysesByMonth(int month) {
    return repository.getAnalysesByMonth(month);
}

bool Person::isIll(int month) {
    std::vector<MedicalAnalysis *> analyses = repository.getAnalysesByMonth(month);
    for (auto &analysis : analyses) {
        if (analysis->isResultOk()) {
            return false;
        }
    }
    return true;
}

std::vector<MedicalAnalysis *> Person::getAnalysesBetweenDates(std::string &date1, std::string &date2) {
    std::vector<MedicalAnalysis *> analyses = repository.getAllAnalyses();
    std::vector<MedicalAnalysis *> result;
    for (auto &analysis : analyses) {
        if (analysis->getDate() >= date1 && analysis->getDate() <= date2) {
            result.push_back(analysis);
        }
    }
    return result;
}

void Person::writeToFile(std::string filename, std::string date1, std::string date2) {
    std::vector<MedicalAnalysis *> analyses = getAnalysesBetweenDates(date1, date2);
    std::ofstream file (filename);
    for (auto &analysis : analyses) {
        file << analysis->toString() << '\n';
    }
    file.close();
}

Person::~Person() = default;