#include "Repository.h"


class Service {
    private:
        Repository &repository;

    public:
        Service(Repository &repository) : repository(repository) {}

        void addAnalysis(MedicalAnalysis *analysis);

        std::vector<MedicalAnalysis *> getAllAnalyses();

        std::vector<MedicalAnalysis *> getAnalysesByMonth(int month);

        void writeToFile(std::string filename);

        ~Service() = default;
};