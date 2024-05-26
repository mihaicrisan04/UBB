#pragma once

#include "service.hpp"

#include <QMainWindow>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QWidget>
#include <QComboBox>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>
#include <QMessageBox>


class GUI : public QMainWindow {
    Q_OBJECT

    private:
        Service service;

        QWidget* centralWidget;
        QVBoxLayout* mainLayout;

        QListWidget* billsListWidget;
        QCheckBox* unpaidCheckBox;
        QWidget* totalUnpaidWidget;
        QLayout* totalUnpaidLayout;
        QLineEdit* companyNameLineEdit;
        QPushButton* calculateTotalUnpaidButton;
        QLabel* totalUnpaidLabel;

    public:
        GUI(std::string filename);
        void initGUI();
        void populateList();
        void populateListUnpaid();
        void connectSignalsAndSlots();
        void calculateTotalUnpaid();
        ~GUI();
};
