#include "UI.h"
#include <iostream>


UI::UI(Person &person) : person(person) {}  

void UI::printMenu() {
    std::cout << "1. Add analysis\n";
    std::cout << "2. Get all analyses\n";
    std::cout << "3. Show if person is ill\n";
    std::cout << "4. Save the file with analyses between two dates\n";
    std::cout << "0. Exit\n";
}

void UI::addAnalysis() {
    std::string date, type, result;
    std::cout << "Date: ";
    std::cin >> date;
    std::cout << "Type: ";
    std::cin >> type;

    MedicalAnalysis *analysis;

    if (type == "bp") {
        int systolicValue, diastolicValue;
        std::cout << "Systolic value: ";
        std::cin >> systolicValue;
        std::cout << "Diastolic value: ";
        std::cin >> diastolicValue;
        analysis = new BP(date, systolicValue, diastolicValue);

    }
    else if (type == "bmi") {
        double value;
        std::cout << "Value: ";
        std::cin >> value;
        analysis = new BMI(date, value);
    }
    else {
        std::cout << "Invalid type\n";
        return;
    }

    person.addAnalysis(analysis);
}

void UI::getAllAnalyses() {
    std::vector<MedicalAnalysis *> analyses = person.getAllAnalyses();
    for (auto &analysis : analyses) {
        std::cout << analysis->toString() << '\n';
    }
}

void UI::showIfPersonIsIll() {
    int month;
    std::cout << "Month: ";
    std::cin >> month;

    if (person.isIll(month)) {
        std::cout << "The person is ill\n";
    } else {
        std::cout << "The person is not ill\n";
    }
}

void UI::saveFileWithAnalysesBetweenTwoDates() {
    std::string date1, date2;
    std::cout << "Date1: ";
    std::cin >> date1;
    std::cout << "Date2: ";
    std::cin >> date2;

    person.writeToFile(date1, date2);
}


void UI::run() {
    person.addAnalysis(new BP("2021.03.01", 100, 60));
    person.addAnalysis(new BP("2021.03.03", 110, 70));
    person.addAnalysis(new BP("2021.03.05", 119, 79));
    person.addAnalysis(new BP("2021.04.06", 100, 70));
    person.addAnalysis(new BP("2021.04.07", 130, 90));
    person.addAnalysis(new BMI("2021.03.02", 18.5));
    person.addAnalysis(new BMI("2021.03.04", 20.0));
    person.addAnalysis(new BMI("2021.03.06", 25));
    person.addAnalysis(new BMI("2021.04.05", 21));
    person.addAnalysis(new BMI("2021.04.08", 30));


    while (true) {
        int command;

        printMenu();

        std::cout << ">>>";
        std::cin >> command;

        if (command == 0) {
            break;
        }
        switch (command) {
            case 1:
                addAnalysis();
                break;

            case 2:
                getAllAnalyses(); 
                break;  

            case 3:
                showIfPersonIsIll();
                break;

            case 4: 
                saveFileWithAnalysesBetweenTwoDates();
                break;

            default:
                std::cout << "Invalid command\n";
        }
    }
}
