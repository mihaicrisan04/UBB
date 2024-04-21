#include <string>
#include <iostream>
#include "Repository.h"

class Person {
public:
    std::string name;
    Repository repository;

private:
    Person(std::string &name);

};