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

#include "session.hpp"  


class Window : public QWidget, public Observer {
private:
    Session &session;
    Courier courier;

    QVBoxLayout *mainLayout;

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
};