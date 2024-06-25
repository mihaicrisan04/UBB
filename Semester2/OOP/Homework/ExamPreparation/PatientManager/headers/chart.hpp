#pragma once

#include <string>
#include <vector>

#include <QWidget>
#include <QVBoxLayout>
#include <QLabel>
#include <QtCharts/QPieSeries>
#include <QtCharts/QPieSlice>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>

#include "session.hpp"


class Chart: public QWidget, public Observer {
    private:
        Session &session;
        QVBoxLayout *layout = new QVBoxLayout(this); // Ensure you have a layout
        QChart *chart;
        QChartView *chartView;
        std::map<std::string, int> data;

    public:
        Chart(Session &session) : session{session} {
            session.addObserver(this);

            chart = new QChart();
            chartView = new QChartView(chart);

            layout->addWidget(chartView);

            update();
            setLayout(layout);
        }

        void update() override {
            data.clear();
            for (auto &patient: session.getPatients()) {
                data[patient.specialty]++;
            }

            QPieSeries *series = new QPieSeries();
            for (auto &entry: data) {
                series->append(QString::fromStdString(entry.first), entry.second);
            }
            chart->removeAllSeries(); 
            chart->addSeries(series);
            chart->setTitle("Patients by specialty");
            chart->legend()->hide();
            chart->setAnimationOptions(QChart::SeriesAnimations);
        }
};