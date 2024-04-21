#include <vector>
#include <string>
#include "MedicalAnalysis.h"
#include "BP.h"
#include "BMI.h"

class Repository {
private:
    std::vector<MedicalAnalysis *> medicalAnalyses;

public:
    Repository();

    void addAnalysis(MedicalAnalysis *analysis);

    std::vector<MedicalAnalysis *> getAllAnalyses();

    std::vector<MedicalAnalysis *> getAnalysesByMonth(int month);

    void writeToFile(std::string filename);
};