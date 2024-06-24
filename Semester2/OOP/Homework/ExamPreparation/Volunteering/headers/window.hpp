#pragma once

#include <QLabel>
#include <QLineEdit>
#include <QTextEdit>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QWidget>

#include "service.hpp"


class Window: public Observer, public QWidget {
    private:
        Service &service;
        Department department;
        QLabel *description;

        QWidget *centralWidget;
        QVBoxLayout *layout;

        QWidget *volunteersWidget;
        QHBoxLayout *volunteersLayout;
        QListWidget *volunteersList, *unassignedList;

        QWidget *addVolunteerWidget;
        QHBoxLayout *addVolunteerLayout;
        QLineEdit *nameEdit, *emailEdit, *interestsEdit;
        QPushButton *addVolunteerButton;

        QPushButton *assignVolunteerButton;

        QWidget *searchWidget;
        QHBoxLayout *searchLayout;
        QPushButton *searchButton;
        QLabel *response;

    public:
        Window(Service &service, Department department, QWidget *parent = nullptr);
        ~Window() = default;

        void update() override;
        void initGUI();
        void connectSignals();

    public slots:
        void addVolunteer();
        void assignVolunteer();
        void search();
};