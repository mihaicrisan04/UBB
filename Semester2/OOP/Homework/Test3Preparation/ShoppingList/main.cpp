#include <iostream>

#include "gui.hpp"
#include <QApplication>


int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    GUI gui("../items.txt");
    gui.show();

    return app.exec();
}

