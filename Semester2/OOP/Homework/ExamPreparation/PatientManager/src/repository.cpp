#include "repository.hpp"

#include <fstream>
#include <sstream>

Repository::Repository() {
    loadDoctors("../doctors.txt");
    loadPatients("../patients.txt");
}

Repository::~Repository() {
    saveDoctors("../doctors.txt");
    savePatients("../patients.txt");
}

void Repository::loadPatients(std::string filename) {
    std::ifstream file (filename);
    std::string line;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string name, diagnosis, specialty, doctor, date;

        std::getline(ss, name, '|');
        std::getline(ss, diagnosis, '|');
        std::getline(ss, specialty, '|');
        std::getline(ss, doctor, '|');
        std::getline(ss, date, '|');

        Patient patient(name, diagnosis, specialty, doctor, date);
        addPatient(patient);
    }

    file.close();
}

void Repository::loadDoctors(std::string filename) {
    std::ifstream file (filename);
    std::string line;

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string name, specialty;

        std::getline(ss, name, '|');
        std::getline(ss, specialty, '|');

        Doctor doctor(name, specialty);
        addDoctor(doctor);
    }

    file.close();
}

void Repository::savePatients(std::string filename) {
    std::ofstream file (filename);

    for (Patient patient : patients) {
        file << patient.toString() << std::endl;
    }

    file.close();
}

void Repository::saveDoctors(std::string filename) {
    std::ofstream file (filename);

    for (Doctor doctor : doctors) {
        file << doctor.toString() << std::endl;
    }

    file.close();
}

void Repository::addPatient(Patient patient) {
    patients.push_back(patient);
}

void Repository::addDoctor(Doctor doctor) {
    doctors.push_back(doctor);
}

std::vector<Patient>& Repository::getPatients() {
    return patients;
}

std::vector<Doctor>& Repository::getDoctors() {
    return doctors;
}