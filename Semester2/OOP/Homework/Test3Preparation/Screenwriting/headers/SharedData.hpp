#ifndef SHAREDDATA_H
#define SHAREDDATA_H

#include <QObject>
#include <QStringList>

class SharedData : public QObject {
    Q_OBJECT

public:
    SharedData();

    const QStringList& getData() const;
    void setData(const QStringList& newData);

signals:
    void dataChanged(const QStringList& newData);

private:
    QStringList data;
};

#endif // SHAREDDATA_H
