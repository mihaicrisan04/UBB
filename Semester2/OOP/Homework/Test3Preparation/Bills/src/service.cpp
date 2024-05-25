#include "service.hpp"


Service::Service(std::string filename) : repository(filename) {}

void Service::addBill(Bill bill) {
    this->repository.addBill(bill);
}

void Service::payBill(std::string serialNumber) {
    this->repository.payBill(serialNumber);
}

std::vector<Bill> Service::getBills() {
    return this->repository.getBills();
}