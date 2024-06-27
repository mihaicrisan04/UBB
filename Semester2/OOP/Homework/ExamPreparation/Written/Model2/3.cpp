#include <iostream>
#include <vector>
#include <string>

using namespace std;

class Handrail {
public:
    virtual ~Handrail() = default;
    virtual double computePrice() = 0;
};

class WoodHandrail : public Handrail {
public:
    double computePrice() override {
        return 2.0;
    }

};

class MetalHandrail : public Handrail {
public:
    double computePrice() override {
        return 2.5;
    }
};

class HandrailWithVerticalElements : public Handrail {
private:
    int noOfElements;
public: 
    HandrailWithVerticalElements(int noOfElements) : noOfElements(noOfElements) {}
    double computePrice() override {
        return 5.0 * noOfElements;
    }
};

class Stair {
private:
    int noOfSteps;
public:
    Stair(int noOfSteps) : noOfSteps(noOfSteps) {}
    virtual ~Stair() = default;
    virtual double getPrice() {
        return noOfSteps * getCoefficient();
    }
    virtual string getDescription() = 0;
    virtual double getCoefficient() = 0;

};

class WoodStair : public Stair {
private:
public:
    WoodStair(int noOfSteps) : Stair(noOfSteps) {}

    string getDescription() override {
        return "Wood stair";
    }

    double getCoefficient() override {
        return 1.5;
    }
};

class MetalStair : public Stair {
private:
public:
    MetalStair(int noOfSteps) : Stair(noOfSteps) {}

    string getDescription() override {
        return "Metal stair";
    }
    
    double getCoefficient() override {
        return 2.0;
    }
};

class StairWithHandrail : public Stair {
private:
    Handrail* handrail;
public:
    StairWithHandrail(int noOfSteps, Handrail* handrail) : Stair(noOfSteps), handrail(handrail) {}
    ~StairWithHandrail() {
        delete handrail;
    }
    double getPrice() override {
        return Stair::getPrice() + handrail->computePrice();
    }

    string getDescription() override {
        return Stair::getDescription();
    }
};

int main() {

    return 0;
}