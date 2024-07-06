#include <iostream>
#include <QApplication>

#include "window.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    
    Repository repo;
    Session session(repo);

    for (auto user: repo.getUsers()) {
        Window *window = new Window(session, user);
        window->show();
    }

    return app.exec();
}
