#include <assert.h>
#include <iostream>

#include "ListIterator.h"
#include "SortedIndexedList.h"

using namespace std;

bool relation1(TComp e1, TComp e2) {
	if (e1 <= e2) {
		return true;
	}
	else {
		return false;
	}
}

void testAll(){
	SortedIndexedList list = SortedIndexedList(relation1);

    // list.add(6);
    // list.add(2);
    // list.add(4);
    // list.add(3);
    // list.add(5);
    // list.add(1);
    // list.add(7);
    // list.add(6);
    // list.add(8);

    // ListIterator iterator = list.iterator();
    // iterator.first();
    // while (iterator.valid()) {
    //     cout << iterator.getCurrent() << " ";
    //     iterator.next();
    // }

    // std::cout << std::endl;
    // std::cout << list.getElement(3) << std::endl;
    // std::cout << list.search(4) << std::endl;
    // std::cout << list.getElement(0) << std::endl;
    // std::cout << list.search(6) << std::endl;
    // std::cout << list.getElement(4) << std::endl;
    // std::cout << list.search(5) << std::endl;
    // std::cout << list.size() << std::endl;

    // std::cout << list.remove(4) << std::endl;

    // iterator.first();
    // while (iterator.valid()) {
    //     cout << iterator.getCurrent() << " ";
    //     iterator.next();
    // }



	assert(list.size() == 0);
	assert(list.isEmpty());
    list.add(1);
    assert(list.size() == 1);
    assert(!list.isEmpty());
    ListIterator iterator = list.iterator();
    assert(iterator.valid());
    iterator.first();
    assert(iterator.getCurrent() == 1);
    iterator.next();
    assert(!iterator.valid());
    iterator.first();
    assert(iterator.valid());
    assert(list.search(1) == 0);
    assert(list.remove(0) == 1);
    assert(list.size() == 0);
    assert(list.isEmpty());
}

