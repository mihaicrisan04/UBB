#include <iostream>
#include "dlla.hpp"
#include <assert.h>


int main() {
    DLLA<int> list;

    for (int i = 0; i < 20; i++) {
        list.add(i);
    }
    std::vector<int> v = list.toVector();
    for (int i: v) {
        std::cout << i << " ";
    }
    std::cout << '\n';
    for (int i = 0; i < 20; i++) {
        assert(list.remove(i) == true);
    }
    return 0;
}