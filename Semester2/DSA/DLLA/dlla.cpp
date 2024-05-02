#include <iostream>
#include "dlla.hpp"
#include <assert.h>


int main() {
    DLLA<int> list;

    for (int i = 0; i < 20; i++) {
        list.add(i);
    }
    for (int i = 0; i < 20; i++) {
        assert(list.remove(i) == true);
    }
    return 0;
}