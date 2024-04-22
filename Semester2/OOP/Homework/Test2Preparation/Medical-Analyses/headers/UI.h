#pragma once
#include "Person.h"


class UI {
    private:
        Person person;

    public:
        UI(Person &person);

        void printMenu();

        void run();

        void addAnalysis();

        void getAllAnalyses();

        void showIfPersonIsIll();

        void saveFileWithAnalysesBetweenTwoDates();

        ~UI() = default;
};