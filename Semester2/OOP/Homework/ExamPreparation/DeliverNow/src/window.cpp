#include "window.hpp"   


void Window::initGUI() {
    mainLayout = new QVBoxLayout{this};

    std::string zone = "Zone: " + std::to_string(std::get<0>(courier.zone)) + " - " + std::to_string(std::get<1>(courier.zone)) + " - " + std::to_string(std::get<2>(courier.zone));
    zoneL = new QLabel{QString::fromStdString(zone)};

    streetsCB = new QComboBox{};
    streetsCB->addItem("Any");
    for (auto street : session.getStreetsOf(courier)) {
        streetsCB->addItem(QString::fromStdString(street));
    }

    packagesWL = new QListWidget{};

    deliverB = new QPushButton{"Deliver"};
    deliverB->setDisabled(true);

    mainLayout->addWidget(zoneL);
    mainLayout->addWidget(streetsCB);
    mainLayout->addWidget(packagesWL);
    mainLayout->addWidget(deliverB);

    setLayout(mainLayout);
    update();
}

void Window::connectSignals() {
    connect(streetsCB, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &Window::update);
    connect(deliverB, &QPushButton::clicked, this, &Window::deliverPackage);
    connect(packagesWL, &QListWidget::itemSelectionChanged, [this]() {
        deliverB->setDisabled(packagesWL->selectedItems().empty());
    });
}

void Window::update() {
    packagesWL->clear();

    std::string address = streetsCB->currentText().toStdString();

    for (auto package : session.getPackagesOf(courier)) {
        if (package.address  == address || address == "Any")  {
            std::string packageStr = package.recipient + " - " + package.address + " - " + std::to_string(std::get<0>(package.location)) + " - " + std::to_string(std::get<1>(package.location));
            QListWidgetItem *item = new QListWidgetItem{QString::fromStdString(packageStr)};
            packagesWL->addItem(item);
        }
    }
}

void Window::deliverPackage() {
    std::string packageStr = packagesWL->selectedItems().at(0)->text().toStdString();
    std::string recipientName = packageStr.substr(0, packageStr.find(" - "));

    session.updatePackageDeliveredStatusOf(recipientName);

    deliverB->setDisabled(true);
}

void CompanyWindow::initGUI() {
    mainLayout = new QVBoxLayout{this};

    packagesWL = new QListWidget{};
    addPackageWidget = new QWidget{};
    addPackageLayout = new QHBoxLayout{addPackageWidget};
    recipientLE = new QLineEdit{};
    addressLE = new QLineEdit{};
    locationLE = new QLineEdit{};
    addPackageB = new QPushButton{"Add"};
    recipientLE->setPlaceholderText("Recipient");
    addressLE->setPlaceholderText("Address");
    locationLE->setPlaceholderText("Location");
    addPackageLayout->addWidget(recipientLE);
    addPackageLayout->addWidget(addressLE);
    addPackageLayout->addWidget(locationLE);
    addPackageLayout->addWidget(addPackageB);

    mainLayout->addWidget(packagesWL);
    mainLayout->addWidget(addPackageWidget);

    setLayout(mainLayout);
    update();
}

void CompanyWindow::connectSignals() {
    connect(addPackageB, &QPushButton::clicked, this, &CompanyWindow::addPackage);
}

void CompanyWindow::update() {
    packagesWL->clear();

    for (auto package : session.getPackages()) {
        std::string packageStr = package.recipient + " - " + package.address + " - " + std::to_string(std::get<0>(package.location)) + " - " + std::to_string(std::get<1>(package.location)) + " - " + (package.delivered ? "Delivered" : "Not delivered");
        QListWidgetItem *item = new QListWidgetItem{QString::fromStdString(packageStr)};
        if (package.delivered) item->setBackground(QBrush(Qt::green));
        packagesWL->addItem(item);
    }
}

void CompanyWindow::addPackage() {
    std::string recipient = recipientLE->text().toStdString();
    std::string address = addressLE->text().toStdString();
    std::string locationStr = locationLE->text().toStdString();

    std::stringstream locationStream(locationStr);
    int x, y;
    std::string token;
    std::getline(locationStream, token, ',');
    x = std::stoi(token);
    std::getline(locationStream, token, ',');
    y = std::stoi(token);

    Package package{recipient, address, {x, y}, false};
    session.addPackage(package);

    recipientLE->clear();
    addressLE->clear();
    locationLE->clear();
}

void MapWindow::initGUI() {
    mainLayout = new QVBoxLayout{this};

    setLayout(mainLayout);
    update();
}

void MapWindow::update() {

}

void MapWindow::paintEvent(QPaintEvent *event) {
    QWidget::paintEvent(event);
    QPainter painter(this);
    for (auto package : session.getPackages()) {
        painter.drawEllipse(QPointF(std::get<0>(package.location) * 10, std::get<1>(package.location)) * 10, 5, 5);
    }
}