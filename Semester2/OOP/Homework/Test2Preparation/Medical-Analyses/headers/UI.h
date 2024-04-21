#include "Person.h"
#include <iostream>


class UI {
    private:
        Person person;

    public:
        UI(Person &person) : person(person) {}

        void printMenu();

        void run();

        void addAnalysis();

        void getAllAnalyses();

        void showIfPersonIsIll();

        void saveFileWithAnalysesBetweenTwoDates();

        ~UI() = default;
};