#include <iostream>

#include <QApplication> 

#include "window.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    Repository repo;
    Session session(repo);

    for (auto researcher : session.getResearchers()) {
        Window *window = new Window(session, researcher);
        window->show();
    }

    return app.exec();
}
