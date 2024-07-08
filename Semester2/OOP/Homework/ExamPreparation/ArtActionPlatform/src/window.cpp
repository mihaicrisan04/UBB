#include "window.hpp"

#include <QMessageBox>


void Window::initGUI() {
    mainLayout = new QVBoxLayout(this);

    itemsList = new QListWidget();
    categoriesComboBox = new QComboBox();

    if (user.type == "administrator") {
        addItemWidget = new QWidget();  
        addItemLayout = new QHBoxLayout(addItemWidget);
        nameLineEdit = new QLineEdit();
        categoryLineEdit = new QLineEdit();
        priceLineEdit = new QLineEdit();
        addItemButton = new QPushButton("Add Item");
        nameLineEdit->setPlaceholderText("Name");
        categoryLineEdit->setPlaceholderText("Category");
        priceLineEdit->setPlaceholderText("Price");

        addItemLayout->addWidget(nameLineEdit);
        addItemLayout->addWidget(categoryLineEdit);
        addItemLayout->addWidget(priceLineEdit);
        addItemLayout->addWidget(addItemButton);
    }

    offersList = new QListWidget();

    mainLayout->addWidget(categoriesComboBox);
    mainLayout->addWidget(itemsList);
    if (user.type == "administrator") {
        mainLayout->addWidget(addItemWidget);
    }
    mainLayout->addWidget(offersList);

    setLayout(mainLayout);
    update();
}

void Window::connectSignals() {
    connect(categoriesComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &Window::update);
    if (user.type == "administrator") {
        connect(addItemButton, &QPushButton::clicked, this, &Window::addItem);
    }
    connect(itemsList, &QListWidget::itemClicked, this, &Window::updateOffers);
}

void Window::update() {
    categoriesComboBox->clear();
    categoriesComboBox->addItem("All");
    std::vector<std::string> categories = session.getCategories();
    for (auto category: categories) {
        categoriesComboBox->addItem(QString::fromStdString(category));
    } 

    itemsList->clear();
    std::vector<Item> items = session.getItems();
    sort(items.begin(), items.end(), [](Item a, Item b) {
        return a.price < b.price;
    });
    std::string category = categoriesComboBox->currentText().toStdString();
    for (auto item: items) {
        if (category != "All" && item.category != category) {
            continue;
        }
        QListWidgetItem* itm = new QListWidgetItem(QString::fromStdString(item.name + " - " + std::to_string(item.price) + " - " + item.category));
        itemsList->addItem(itm);
    }
}

void Window::addItem() {
    std::string name = nameLineEdit->text().toStdString();
    std::string category = categoryLineEdit->text().toStdString();
    int price = priceLineEdit->text().toInt();
    if (name.empty() || category.empty() || price <= 0) {
        QMessageBox::warning(this, "Error", "Invalid data");
        return;
    }
    Item item(name, category, price);
    std::cout << item.name << " " << item.category << " " << item.price << std::endl;
    std::cout << "Offers: " << item.offers.size() << std::endl;
    
    session.addItem(item);
}

void Window::updateOffers() {
    offersList->clear();
    QListWidgetItem* itm = itemsList->currentItem();
    if (!itm) {
        return;
    }
    std::string itemName = itm->text().toStdString();
    std::string name = itemName.substr(0, itemName.find(" - "));
    for (auto item: session.getItems()) {
        if (item.name == name) {
            for (auto offer: item.offers) {
                std::string offerStr = std::to_string(std::get<0>(offer)) + " - " + std::get<1>(offer) + " - " + std::to_string(std::get<2>(offer));
                offersList->addItem(QString::fromStdString(offerStr));
            }
        }
    }
}
