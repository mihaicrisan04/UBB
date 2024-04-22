#pragma once
#include <string>
#include <vector>
#include "BMI.h"
#include "BP.h"


class Person {
private:
    std::string name;
    std::vector<MedicalAnalysis *> analyses;

public:
    Person(std::string name);

    void addAnalysis(MedicalAnalysis *analysis);

    std::vector<MedicalAnalysis *> getAllAnalyses();

    std::vector<MedicalAnalysis *> getAnalysesByMonth(int month);

    bool isIll(int month);

    std::vector<MedicalAnalysis *> getAnalysesBetweenDates(std::string &date1, std::string &date2);

    void writeToFile(std::string date1, std::string date2);

    ~Person();
};