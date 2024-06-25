#pragma once

#include "repository.hpp"
#include "subject.hpp"


class Session: public Subject {
    private:
        Repository &repo;

    public:
        Session(Repository &repo);

        void addPatient(Patient patient);

        std::vector<Patient> getPatientsBy(std::string specialty);
        Patient getPatientByName(std::string name);
        void updatePatient(std::string name, std::string diagnosis, std::string specialty);

        std::vector<Patient>& getPatients();
        std::vector<Doctor>& getDoctors();

};