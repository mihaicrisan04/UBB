#include "Person.h"
#include <fstream>
#include <string>


Person::Person(std::string name) : name(name), analyses() {}

void Person::addAnalysis(MedicalAnalysis *analysis) {
    analyses.push_back(analysis);
}

std::vector<MedicalAnalysis *> Person::getAllAnalyses() {
    return analyses;
}

std::vector<MedicalAnalysis *> Person::getAnalysesByMonth(int month) {
    std::vector<MedicalAnalysis *> result;
    for (auto &analysis : analyses) {
        if (stoi(analysis->getDate().substr(5, 2)) == month) {
            result.push_back(analysis);
        }
    }
    return result;
}

bool Person::isIll(int month) {
    for (auto &analysis : analyses) {
        if (stoi(analysis->getDate().substr(5, 2)) == month) {
            if (analysis->isResultOK() == false) {
                return true;
            }
        }
    }
    return false;
}

std::vector<MedicalAnalysis *> Person::getAnalysesBetweenDates(std::string &date1, std::string &date2) {
    std::vector<MedicalAnalysis *> result;
    for (auto &analysis : analyses) {
        if (analysis->getDate() >= date1 && analysis->getDate() <= date2) {
            result.push_back(analysis);
        }
    }
    return result;
}

void Person::writeToFile(std::string date1, std::string date2) {
    std::string filename = "../" + name + "_" + date1 + "-" + date2 + ".txt";  // e.g. "John_2021.03.01-2021.03.31.txt"
    std::ofstream file(filename);
    for (auto &analysis : analyses) {
        if (analysis->getDate() >= date1 && analysis->getDate() <= date2) {
            file << analysis->toString() << '\n';
        }
    }
    file.close();
}

Person::~Person() {
    for (auto &analysis : analyses) {
        delete analysis;
    }
}