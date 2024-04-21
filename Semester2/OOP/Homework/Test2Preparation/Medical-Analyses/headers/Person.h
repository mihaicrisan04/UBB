#include <string>
#include <iostream>
#include "Repository.h"

class Person {
private:
    std::string name;
    Repository repository;

public:
    Person(std::string &name);

    void addAnalysis(MedicalAnalysis *analysis);

    std::vector<MedicalAnalysis *> getAllAnalyses();

    std::vector<MedicalAnalysis *> getAnalysesByMonth(int month);

    bool isIll(int month);

    std::vector<MedicalAnalysis *> getAnalysesBetweenDates(std::string &date1, std::string &date2);

    void writeToFile(std::string filename, std::string date1, std::string date2);

    ~Person();
};