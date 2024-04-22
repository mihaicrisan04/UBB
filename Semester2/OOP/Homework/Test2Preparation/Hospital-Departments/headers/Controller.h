#pragma once
#include "NeonatalUnit.h"
#include "Surgery.h"
#include <vector>
#include <fstream>
#include <iostream>
#include <string>


class Controller {
    private:
        std::vector<HospitalDepartment *> departments;
        
    public:
        Controller(); 

        void addDepartment(HospitalDepartment *department);

        std::vector<HospitalDepartment *> getAllDepartments() const;

        std::vector<HospitalDepartment *> getAllEfficientDepartments() const;

        void writeToFile(std::string filename) const;

        ~Controller();
};