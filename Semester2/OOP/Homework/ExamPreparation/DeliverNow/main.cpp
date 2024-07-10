#include <iostream>

#include <QApplication>

#include "window.hpp"


int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    Repository repo;
    Session session{repo};  

    for (auto courier : repo.getCouriers()) {
        Window *window = new Window{session, courier};
        window->show();
    }
    

    return app.exec();
}
