#include <iostream>

#include <QApplication>
#include <QMainWindow>

#include "service.hpp"
#include "window.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    Repository repo;
    Service service(repo);    

    for (Department department: service.getDepartments()) {
        Window *window = new Window(service, department);
        window->show();
    }

    return app.exec();
}
