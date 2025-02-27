#pragma once

#include "session.hpp"
#include "observer.hpp" 

#include <QWidget>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QListWidget>
#include <QListWidgetItem>
#include <QComboBox>    


class Window: public QWidget, public Observer {
private:
    Session &session;
    User user;

    QVBoxLayout *mainLayout;    

    QListWidget* itemsList;
    QComboBox* categoriesComboBox;

    QWidget* addItemWidget;
    QHBoxLayout* addItemLayout;
    QLineEdit* nameLineEdit;
    QLineEdit* categoryLineEdit;
    QLineEdit* priceLineEdit;
    QPushButton* addItemButton;

    QListWidget* offersList;


public:
    Window(Session &session, User user, QWidget *parent = nullptr): session(session), user(user), QWidget(parent) {
        session.addObserver(this);
        setWindowTitle(QString::fromStdString(user.name));
        setMinimumSize(400, 200);
        initGUI();
        connectSignals();
    }

    void initGUI();
    void connectSignals();  

    void update() override;
    
public slots:
    void addItem();
    void updateOffers();
};