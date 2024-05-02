#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>

SMMIterator::SMMIterator(const SortedMultiMap& d) : map(d){
	currentKey = map.head;
	if (currentKey != -1) {
		currentVal = 0;
		values = map.arr[currentKey].values.toVector();
	}
	else {
		currentVal = -1;
		values = std::vector<TValue>();
	}
}

void SMMIterator::first(){
	currentKey = map.head;
	if (currentKey != -1) {
		currentVal = 0;
		values = map.arr[currentKey].values.toVector();
	}
	else {
		currentVal = -1;
		values = std::vector<TValue>();
	}
}

void SMMIterator::next(){
	if (currentKey == -1) {
		throw std::exception();
	}
	// std::cout << currentKey << " " << map.arr[currentKey].next << " " << values.size() << " " << map.arr[currentKey].key << " " << values[currentVal] << "\n";

	if (currentVal < values.size() - 1) {
		currentVal++;
	}
	else {
		currentKey = map.arr[currentKey].next;
		if (currentKey != -1) {
			currentVal = 0;
			values = map.arr[currentKey].values.toVector();
		}
	}
}

bool SMMIterator::valid() const{
	return currentKey != -1;
}

TElem SMMIterator::getCurrent() const{
	if (!valid()) {
		throw std::exception();
	}
	return TElem(map.arr[currentKey].key, values[currentVal]);
}


