#pragma once

#include "service.hpp"

#include <QMainWindow>
#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>


class GUI: public QMainWindow {
    Q_OBJECT

    private:
        Service serv;

        QWidget *centralWidget;
        QVBoxLayout *mainLayout;

        QListWidget *itemsList;
        QPushButton *deleteButton;

        QWidget *filterWidget;
        QHBoxLayout *filterLayout;
        QLineEdit *categoryLineEdit;
        QPushButton *filterButton;

    public:
        GUI(std::string filename);
        ~GUI();

        void initGUI();
        void connectSignalsAndSlots();
        void populateList();
        void populateListByCategory(std::string category);
};
