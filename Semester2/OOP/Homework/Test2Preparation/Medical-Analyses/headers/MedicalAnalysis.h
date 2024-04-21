#include <string>
#include <iostream>


class MedicalAnalysis {
private:
    std::string date;

public:
    MedicalAnalysis(std::string &date) : date(date) {}

    std::string getDate() const {
        return date;
    }

    virtual bool isResultOk() = 0;

    virtual std::string toString() = 0;

    virtual ~MedicalAnalysis() = default;
};