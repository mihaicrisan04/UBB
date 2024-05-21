#include <QApplication>
#include "headers/SharedData.hpp"
#include "headers/ListWindow.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    SharedData sharedData;

    ListWindow window1(&sharedData);
    ListWindow window2(&sharedData);

    window1.setWindowTitle("Window 1");
    window2.setWindowTitle("Window 2");

    window1.show();
    window2.show();

    return app.exec();
}
