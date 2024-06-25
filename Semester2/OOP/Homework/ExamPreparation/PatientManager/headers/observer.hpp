#pragma once


class Observer {
    public:
        Observer() = default;
        virtual ~Observer() = default;
        virtual void update() = 0;
};