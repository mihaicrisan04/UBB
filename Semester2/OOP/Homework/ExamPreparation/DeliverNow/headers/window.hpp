#pragma once

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>  
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>
#include <QMessageBox>
#include <QComboBox>    

#include <QPainter>

#include "session.hpp"  


class Window : public QWidget, public Observer {
private:
    Session &session;
    Courier courier;

    QVBoxLayout *mainLayout;

    QLabel *zoneL;
    QListWidget *packagesWL; 
    QComboBox *streetsCB;
    QPushButton *deliverB;


public:
    Window(Session &session, Courier courier, QWidget *parent = nullptr) : session(session), courier(courier), QWidget(parent) {
        session.addObserver(this);
        setWindowTitle(QString::fromStdString(courier.name));
        setMinimumSize(400, 200);
        initGUI();
        connectSignals();
    }

    void initGUI();
    void connectSignals();

    void update() override;

public slots:
    void deliverPackage();
    
};

class CompanyWindow : public QWidget, public Observer {
private:
    Session &session;

    QVBoxLayout *mainLayout;

    QListWidget *packagesWL;

    QWidget *addPackageWidget;
    QHBoxLayout *addPackageLayout;
    QLineEdit *recipientLE;
    QLineEdit *addressLE;
    QLineEdit *locationLE;
    QPushButton *addPackageB;

public:
    CompanyWindow(Session &session, QWidget *parent = nullptr) : session(session), QWidget(parent) {
        session.addObserver(this);
        setWindowTitle("Company");
        setMinimumSize(400, 200);
        initGUI();
        connectSignals();
    }

    void initGUI();
    void connectSignals();

    void update() override;
public slots:
    void addPackage();
};

class MapWindow : public QWidget, public Observer { 
private:
    Session &session;

    QVBoxLayout *mainLayout;

public:
    MapWindow(Session &session, QWidget *parent = nullptr) : session(session), QWidget(parent) {
        session.addObserver(this);
        setWindowTitle("Map");
        setMinimumSize(400, 200);
        initGUI();
    }

    void initGUI();

    void paintEvent(QPaintEvent *event) override;

    void update() override;
};