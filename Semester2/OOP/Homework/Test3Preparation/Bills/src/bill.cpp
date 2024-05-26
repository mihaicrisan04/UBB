#include "bill.hpp"

#include <iomanip>  // Required for std::fixed and std::setprecision
#include <sstream>  // Required for std::ostringstream


Bill::Bill(std::string companyName, std::string serialNumber, double amount, bool paid) {
    this->companyName = companyName;
    this->serialNumber = serialNumber;
    this->amount = amount;
    this->paid = paid;
}

void Bill::payBill() {
    this->paid = true;
}

std::string Bill::toString() const {
    std::string amountString = std::to_string(this->amount);
    amountString = amountString.substr(0, amountString.size() - 4);

    return this->companyName + ";" + this->serialNumber + ";" + amountString + ";" + (this->paid ? "true" : "false");
}

Bill::~Bill() {}