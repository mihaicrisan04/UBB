#include "gui.hpp"

GUI::GUI(std::string filename) : service(filename) {
    initGUI();
}

void GUI::initGUI() {
    setWindowTitle("Bills Manager");
    centralWidget = new QWidget{};
    mainLayout = new QVBoxLayout{centralWidget};

    billsListWidget = new QListWidget{};
    unpaidCheckBox = new QCheckBox{"unpaid only"};

    totalUnpaidWidget = new QWidget{};
    totalUnpaidLayout = new QHBoxLayout{totalUnpaidWidget};
    companyNameLineEdit = new QLineEdit{};
    calculateTotalUnpaidButton = new QPushButton{"Calculate"};
    totalUnpaidLabel = new QLabel{"Total unpaid: __"};

    totalUnpaidLayout->addWidget(companyNameLineEdit);
    totalUnpaidLayout->addWidget(calculateTotalUnpaidButton);
    totalUnpaidLayout->addWidget(totalUnpaidLabel);

    mainLayout->addWidget(unpaidCheckBox);
    mainLayout->addWidget(billsListWidget);
    mainLayout->addWidget(totalUnpaidWidget);

    setCentralWidget(centralWidget);
    connectSignalsAndSlots();
    populateList();
}

void GUI::populateList() {
    billsListWidget->clear();
    std::vector<Bill> bills = this->service.getBills();
    sort(bills.begin(), bills.end(), [](const Bill& bill1, const Bill& bill2) {
        return bill1.companyName < bill2.companyName;
    });
    for (const auto& bill : bills) {
        QListWidgetItem* item = new QListWidgetItem(QString::fromStdString(bill.toString()));
        if (!bill.paid) {
            item->setBackground(Qt::red);
        }
        billsListWidget->addItem(item);
    }
    mainLayout->addWidget(billsListWidget);
}

void GUI::populateListUnpaid() {
    billsListWidget->clear();
    std::vector<Bill> bills = this->service.getBills();
    sort(bills.begin(), bills.end(), [](const Bill& bill1, const Bill& bill2) {
        return bill1.companyName < bill2.companyName;
    });
    for (const auto& bill : bills) {
        if (bill.paid) {
            continue;
        }
        QListWidgetItem* item = new QListWidgetItem(QString::fromStdString(bill.toString()));
        item->setBackground(Qt::red);
        billsListWidget->addItem(item);
    }
    mainLayout->addWidget(billsListWidget);
}

void GUI::calculateTotalUnpaid() {
    std::string companyName = companyNameLineEdit->text().toStdString();
    double totalUnpaid = 0;
    for (const auto& bill : this->service.getBills()) {
        if (bill.companyName == companyName && !bill.paid) {
            totalUnpaid += bill.amount;
        }
    }
    if (totalUnpaid == 0) {
        QMessageBox* messageBox = new QMessageBox{};
        messageBox->setText("No unpaid bills for this company");
        messageBox->exec();
        totalUnpaidLabel->setText("Total unpaid: __");
        delete messageBox;
        return;
    }
    totalUnpaidLabel->setText(QString::fromStdString("Total unpaid: " + std::to_string(totalUnpaid)));
}

void GUI::connectSignalsAndSlots() {
    QObject::connect(unpaidCheckBox, &QCheckBox::stateChanged, this, [this]() {
        if (unpaidCheckBox->isChecked()) {
            populateListUnpaid();
        } else {
            populateList();
        }
    });
    QObject::connect(calculateTotalUnpaidButton, &QPushButton::clicked, this, &GUI::calculateTotalUnpaid);
}

GUI::~GUI() {
    delete centralWidget;
}


