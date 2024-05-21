#ifndef LISTWINDOW_H
#define LISTWINDOW_H

#include <QWidget>
#include <QListWidget>
#include "SharedData.hpp"

class ListWindow : public QWidget {
    Q_OBJECT

public:
    ListWindow(SharedData* sharedData, QWidget* parent = nullptr);

public slots:
    void updateList(const QStringList& newData);

private:
    QListWidget* listWidget;
    SharedData* sharedData;
};

#endif // LISTWINDOW_H
