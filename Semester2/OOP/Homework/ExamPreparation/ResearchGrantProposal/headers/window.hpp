#pragma once

#include "session.hpp"

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QTableView>


class Window : public QWidget, public Observer {
    private:
        Session &session;
        Researcher researcher;

        QVBoxLayout *mainLayout;
        QLabel *positionLabel;
        QTableView *ideasTable;


    public:
        Window(Session &session, Researcher researcher, QWidget *parent = nullptr) : session(session), researcher(researcher), QWidget(parent) {
            this->session.addObserver(this);
            initGUI();
            connectSignals();
        }

        void initGUI();
        void connectSignals();

        void update() override; 
    
    public slots:
};