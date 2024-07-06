#pragma once

#include "session.hpp"

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>

#include <QTableView>
#include "ideaModel.hpp"


class Window : public QWidget, public Observer {
    private:
        Session &session;
        Researcher researcher;

        QVBoxLayout *mainLayout;

        QLabel *positionLabel;
        QTableView *ideasTable;
        IdeaModel *model;

        QWidget *addIdeaWidget;
        QHBoxLayout *addIdeaLayout;
        QLineEdit *titleEdit;
        QLineEdit *descriptionEdit;
        QLineEdit *durationEdit;
        QPushButton *addIdeaButton;

        QPushButton *acceptButton;
        QPushButton *saveAllButton;

        QPushButton *developButton;
        bool hasAcceptedIdeas = false;  

    public:
        Window(Session &session, Researcher researcher, QWidget *parent = nullptr) : session(session), researcher(researcher), QWidget(parent) {
            this->session.addObserver(this);
            setMinimumSize(400, 200);
            initGUI();
            connectSignals();
        }

        void initGUI();
        void connectSignals();

        void update() override; 
    
    public slots:
        void addIdea();
        void acceptIdea();
        void saveAll();
        void developIdea();
};