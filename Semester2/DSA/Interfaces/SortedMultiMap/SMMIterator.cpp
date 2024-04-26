#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>

SMMIterator::SMMIterator(const SortedMultiMap& d) : map(d) {
	first();	
}

void SMMIterator::first() {
	if (map.isEmpty()) {
		currentKeyIndex = -1;
		currentValueIndex = -1;
	} else {
		currentKeyIndex = map.head;
		currentValueIndex = map.keys[currentKeyIndex].head;
	}	
}

void SMMIterator::next() {
	if (!valid()) {
		throw std::exception();
	}	
	currentValueIndex = map.keys[currentKeyIndex].values[currentValueIndex].next;
	if (currentValueIndex == -1) {
		currentKeyIndex = map.keys[currentKeyIndex].next;
		if (currentKeyIndex != -1) {
			currentValueIndex = map.keys[currentKeyIndex].head;
		}
	}
}

bool SMMIterator::valid() const {
	return currentKeyIndex != -1 && currentValueIndex != -1;
}

TElem SMMIterator::getCurrent() const {
	if (!valid()) {
		throw std::exception();
	}
	return TElem(map.keys[currentKeyIndex].key, map.keys[currentKeyIndex].values[currentValueIndex].value);
}


