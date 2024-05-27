#pragma once

#include "service.hpp"

#include <QMainWindow>
#include <QWidget>
#include <QLabel>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QLineEdit>
#include <QListWidget>
#include <QListWidgetItem>


class GUI: public QMainWindow {
    Q_OBJECT

    private:
        Service serv;

        QWidget *centralWidget;
        QVBoxLayout *mainLayout;

        QListWidget *familiesList;
        QListWidget *vegetablesList;

        QWidget *searchWidget;
        QHBoxLayout *searchLayout;
        QLineEdit *inputLineEdit;
        QPushButton *searchButton;
        QLabel *partsLabel;

    public:
        GUI(std::string filename);
        ~GUI();
        
        void initGUI();
        void connectSignalsAndSlots();
        void populateFamilies();
        void populateVegetablesFrom(std::string family);
};