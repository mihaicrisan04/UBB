#include <iostream>

#include <QApplication>
#include "gui.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    GUI gui("../tasks.txt");
    gui.show();

    return app.exec();
}
