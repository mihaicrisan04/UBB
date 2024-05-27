#include "gui.hpp"

#include <iostream>

GUI::GUI(std::string filename) : serv(filename) {
    initGUI();
}

GUI::~GUI() {
    delete centralWidget;
}

void GUI::initGUI() {
    setWindowTitle("Shopping List");
    centralWidget = new QWidget;
    mainLayout = new QVBoxLayout(centralWidget);

    itemsList = new QListWidget();
    deleteButton = new QPushButton("Delete");

    filterWidget = new QWidget();
    filterLayout = new QHBoxLayout(filterWidget);
    categoryLineEdit = new QLineEdit();
    filterButton = new QPushButton("Filter");

    filterLayout->addWidget(categoryLineEdit);
    filterLayout->addWidget(filterButton);

    categoryLineEdit->setPlaceholderText("i.e. Drinks");

    mainLayout->addWidget(filterWidget);
    mainLayout->addWidget(itemsList);
    mainLayout->addWidget(deleteButton);

    setCentralWidget(centralWidget);
    connectSignalsAndSlots();
    populateList();
}


void GUI::populateList() {
    std::vector<Item> items = serv.getItems();

    for (auto item: items) {
        QListWidgetItem *itm = new QListWidgetItem(QString::fromStdString(item.toString()));
        itemsList->addItem(itm);
    }
}

void GUI::populateListByCategory(std::string category) {
    std::vector<Item> items = serv.getItems();

    for (auto item: items) {
        if (item.category == category) {
            QListWidgetItem *itm = new QListWidgetItem(QString::fromStdString(item.toString()));
            itemsList->addItem(itm);
        }
    }
}

void GUI::connectSignalsAndSlots() {
    connect(deleteButton, &QPushButton::clicked, this, [this]() {
        if (itemsList->selectedItems().isEmpty()) {
            return;
        }
        int index = itemsList->currentRow();
        std::vector<Item> items = serv.getItems();
        Item item = items[index];
        serv.deleteItem(item);
        itemsList->clear();
        populateList();
    });
    connect(filterButton, &QPushButton::clicked, this, [this]() {
        itemsList->clear();
        std::string category = categoryLineEdit->text().toStdString();
        if (category.empty())
            populateList();
        else 
            populateListByCategory(category);
    });
}

