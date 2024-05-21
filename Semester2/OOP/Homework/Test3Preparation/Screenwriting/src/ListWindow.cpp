#include "ListWindow.hpp"
#include <QVBoxLayout>
#include <QLineEdit>
#include <QPushButton>

ListWindow::ListWindow(SharedData* sharedData, QWidget* parent)
    : QWidget(parent), sharedData(sharedData) {
    QVBoxLayout* layout = new QVBoxLayout(this);
    listWidget = new QListWidget(this);
    layout->addWidget(listWidget);

    QLineEdit* input = new QLineEdit(this);
    layout->addWidget(input);

    QPushButton* addButton = new QPushButton("Add", this);
    layout->addWidget(addButton);

    connect(addButton, &QPushButton::clicked, [this, input, sharedData]() {
        QString text = input->text();
        if (!text.isEmpty()) {
            QStringList newData = sharedData->getData();
            newData.append(text);
            sharedData->setData(newData);
            input->clear();
        }
    });

    connect(sharedData, &SharedData::dataChanged, this, &ListWindow::updateList);
    updateList(sharedData->getData());
}

void ListWindow::updateList(const QStringList& newData) {
    listWidget->clear();
    listWidget->addItems(newData);
}
