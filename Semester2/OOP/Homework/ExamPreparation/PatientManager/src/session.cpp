#include "session.hpp"

#include <iostream>

Session::Session(Repository &repo) : repo(repo) {}

void Session::addPatient(Patient patient) {
    repo.addPatient(patient);
    notify();
}

Patient Session::getPatientByName(std::string name) {
    for (Patient &patient : repo.getPatients()) {
        if (patient.name == name) {
            return patient;
        }
    }

    return Patient("not found", "not found", "not found", "not found", "not found");
}

void Session::updatePatient(std::string name, std::string specialty, std::string diagnosis) {
    for (Patient &patient : repo.getPatients()) {
        if (patient.name == name) {
            patient.specialty = specialty;
            patient.diagnosis = diagnosis;
            notify();
            return;
        }
    }
}

std::vector<Patient> Session::getPatientsBy(std::string specialty) {
    std::vector<Patient> patients;

    for (Patient &patient : repo.getPatients()) {
        if (patient.specialty == specialty || patient.diagnosis == "undiagnosed") {
            patients.push_back(patient);
        }
    }

    return patients;
}

std::vector<Patient>& Session::getPatients() {
    return repo.getPatients();
}

std::vector<Doctor>& Session::getDoctors() {
    return repo.getDoctors();
}