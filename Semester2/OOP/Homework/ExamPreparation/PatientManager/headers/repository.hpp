#pragma once

#include <vector>
#include <string>

#include "patient.hpp"
#include "doctor.hpp"

class Repository {
    private:
        std::vector<Patient> patients;
        std::vector<Doctor> doctors;

    public:
        Repository();
        ~Repository();

        void loadPatients(std::string filename);
        void loadDoctors(std::string filename);

        void savePatients(std::string filename);
        void saveDoctors(std::string filename);

        void addPatient(Patient patient);
        void addDoctor(Doctor doctor);  

        std::vector<Patient>& getPatients();
        std::vector<Doctor>& getDoctors();
};