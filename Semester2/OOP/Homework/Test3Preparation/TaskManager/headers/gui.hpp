#pragma once

#include "service.hpp"

#include <QMainWindow>
#include <QWidget>
#include <QListWidget>
#include <QListWidgetItem>
#include <QPushButton>
#include <QLineEdit>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>


class GUI: public QMainWindow {
    Q_OBJECT

    private:
        Service serv;

        QWidget *centralWidget;
        QHBoxLayout *mainLayout;

        QListWidget *tasksList;

        QWidget *durationWidget;
        QVBoxLayout *durationLayout;

        QLineEdit *inputDurationLineEdit;
        QPushButton *computeDurationButton;
        QLabel *responseLabel;

    public:
        GUI(std::string filename);
        ~GUI();

        void initGUI();
        void connectSingalsAndSlots();
        void populateList();
};