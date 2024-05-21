#include "SharedData.hpp"

SharedData::SharedData() : QObject() {}

const QStringList& SharedData::getData() const {
    return data;
}

void SharedData::setData(const QStringList& newData) {
    if (data != newData) {
        data = newData;
        emit dataChanged(data);
    }
}
