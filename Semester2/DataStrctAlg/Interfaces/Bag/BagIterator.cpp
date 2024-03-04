#include <exception>
#include "BagIterator.h"
#include "Bag.h"

using namespace std;


BagIterator::BagIterator(const Bag& c) : bag(c), index(0) {
	//TODO - Implementation
}

void BagIterator::first() {
	index = 0;	
}


void BagIterator::next() {
	if (valid()) {
		index++;
	}
	else {
		throw exception();
	}
}


bool BagIterator::valid() const {
	return index < bag.size();
}



TElem BagIterator::getCurrent() const {
	if (valid()) {
		return bag.array[index];
	}
	else {
		throw exception();
	}
}
