#include "window.hpp"


void Window::initGUI() {
    setWindowTitle(QString::fromStdString(researcher.name));
    mainLayout = new QVBoxLayout(this);

    positionLabel = new QLabel(QString::fromStdString(researcher.position));


    mainLayout->addWidget(positionLabel);


    setLayout(mainLayout);
}

void Window::connectSignals() {

}

void Window::update() {

}