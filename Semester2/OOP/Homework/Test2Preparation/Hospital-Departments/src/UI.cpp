#include "UI.h"


UI::UI(Controller controller) : controller(controller) {}

void UI::printMenu() const {
    std::cout << "1. Add new department\n";
    std::cout << "2. Show all departments\n";
    std::cout << "3. Show all efficient departments\n";
    std::cout << "4. Write to file\n";
    std::cout << "0. Exit\n";
}

void UI::AddNewDepartment() {
    HospitalDepartment *department = NULL;
    std::string type;
    int numDoctors;

    std::cout << "Type: ";
    std::cin >> type;
    std::cout << "Number of Doctors: ";
    std::cin >> numDoctors;


    if (type == "s") {
        // Surgery
        int numPatients;
        std::cout << "Number of Patients: ";
        std::cin >> numPatients;

        department = new Surgery("SurgDep", numDoctors, numPatients);
    }
    else if (type == "n") {
        int numMothers, numNewborns;
        double averageGrade;

        std::cout << "Number of mothers: ";
        std::cin >> numMothers;
        std::cout << "Number of new borns: ";
        std::cin >> numNewborns;
        std::cout << "Average Grade(ex 7.5): ";
        std::cin >> averageGrade;

        department = new NeonatalUnit("NatalDep", numDoctors, numMothers, numNewborns, averageGrade);
    }
    else {
        std::cout << "Invalid type of department\n";
        return;
    }

    controller.addDepartment(department);
}

void UI::ShowAllDepartments() {
    std::vector<HospitalDepartment *> departments = controller.getAllDepartments();
    for (auto &department : departments) {
        std::cout << department->toString() << '\n';
    }
}

void UI::ShowAllEfficientDepartments() {
    std::vector<HospitalDepartment *> departments = controller.getAllEfficientDepartments();
    for (auto &department : departments) {
        std::cout << department->toString() << '\n';
    } 
}

void UI::writeToFile() {
    controller.writeToFile("../hospital.txt");
    std::cout << "Written to file\n";
}

void UI::run() {
    controller.addDepartment(new Surgery("SurgDep", 10, 20));
    controller.addDepartment(new Surgery("SurgDep", 15, 30));
    controller.addDepartment(new Surgery("SurgDep", 5, 5));
    controller.addDepartment(new NeonatalUnit("NatalDep", 5, 10, 15, 9.5));
    controller.addDepartment(new NeonatalUnit("NatalDep", 7, 12, 20, 8.5));
    controller.addDepartment(new NeonatalUnit("NatalDep", 3, 5, 4, 7.5));

    bool running = true;
    while (running) {
        printMenu();

        int command;
        std::cout << ">>>";
        std::cin >> command;

        switch (command) {
        case 0:
            running = false;
            break;

        case 1:
            AddNewDepartment();
            break;

        case 2:
            ShowAllDepartments();
            break;

        case 3:
            ShowAllEfficientDepartments();
            break;

        case 4:
            writeToFile();
            break;
        
        default:
            std::cout << "Invalid command\n";
            break;
        }
    }
}


