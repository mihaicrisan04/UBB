#include "gui.hpp"

#include <algorithm>
#include <iostream>


GUI::GUI(std::string filename) : serv(filename) {
    initGUI();
}

GUI::~GUI() {
    delete centralWidget;
}

void GUI::initGUI() {
    centralWidget = new QWidget;
    mainLayout = new QVBoxLayout(centralWidget);

    familiesList = new QListWidget;
    vegetablesList = new QListWidget;

    searchWidget = new QWidget;
    searchLayout = new QHBoxLayout(searchWidget);
    inputLineEdit = new QLineEdit;
    inputLineEdit->setPlaceholderText("i.e. Potato");
    searchButton = new QPushButton("Search");
    partsLabel = new QLabel("");

    searchLayout->addWidget(inputLineEdit); 
    searchLayout->addWidget(searchButton);
    searchLayout->addWidget(partsLabel);

    mainLayout->addWidget(familiesList);
    mainLayout->addWidget(vegetablesList);
    mainLayout->addWidget(searchWidget);

    setCentralWidget(centralWidget);
    connectSignalsAndSlots();
    populateFamilies();
}

void GUI::populateFamilies() {
    std::vector<std::string> families = serv.getFamilies();
    sort(families.begin(), families.end());

    for (auto f: families) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(f));
        familiesList->addItem(item);
    }
}

void GUI::populateVegetablesFrom(std::string family) {
    std::vector<Vegetable> vegetables = serv.getVegetables();

    for (auto v: vegetables) {
        if (v.family == family) {
            std::string nameAndParts = v.name + " - " + v.parts;
            QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(nameAndParts));
            vegetablesList->addItem(item);
        }
    }
}

void GUI::connectSignalsAndSlots() {
    connect(familiesList, &QListWidget::itemSelectionChanged, this, [this]() {
        if (familiesList->currentItem() != nullptr) {
            std::string family = familiesList->currentItem()->text().toStdString();
            vegetablesList->clear();
            populateVegetablesFrom(family);
        }
    });
    connect(searchButton, &QPushButton::clicked, this, [this]() {
        std::string name = inputLineEdit->text().toStdString();
        Vegetable v = serv.getVegetableBy(name);

        // TODO error handling
        if (v.name == "") {
            inputLineEdit->setText("");
            partsLabel->setText("");
            return;
        }

        familiesList->clear();
        populateFamilies();
        for (int i = 0; i < familiesList->count(); i++) {
            QListWidgetItem *item = familiesList->item(i);
            if (item->text().toStdString() == v.family) {
                item->setSelected(true);
                break;
            }
        }

        partsLabel->setText(QString::fromStdString(v.parts));
    });
}
