#pragma once

#include <vector>
#include "session.hpp"
#include "idea.hpp" 

#include <QAbstractTableModel>  


class IdeaModel: public QAbstractTableModel {
    private:
        std::vector<Idea> ideas;
        Session &session;

    public:
        IdeaModel(Session &session, QObject *parent = nullptr) : QAbstractTableModel(parent), session(session), ideas(session.getIdeas()) {}

        int rowCount(const QModelIndex &parent = QModelIndex()) const override {
            return ideas.size();
        }

        int columnCount(const QModelIndex &parent = QModelIndex()) const override {
            return 5;
        }

        QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override {
            if (role == Qt::DisplayRole) {
                Idea idea = ideas[index.row()];

                switch (index.column()) {
                    case 0:
                        return QString::fromStdString(idea.title);
                    case 1:
                        return QString::fromStdString(idea.description);
                    case 2:
                        return QString::fromStdString(idea.status);
                    case 3:
                        return QString::fromStdString(idea.creator);
                    case 4:
                        return QString::number(idea.duration);
                }
            }

            return QVariant();
        }

        QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override {
            if (role == Qt::DisplayRole && orientation == Qt::Horizontal) {
                switch (section) {
                    case 0:
                        return "Title";
                    case 1:
                        return "Description";
                    case 2:
                        return "Status";
                    case 3:
                        return "Creator";
                    case 4:
                        return "Duration";
                }
            }

            return QVariant();
        }

        void update() {
            beginResetModel();
            ideas = session.getIdeas();
            endResetModel();
        }
};