#include "SMMIterator.h"
#include "SortedMultiMap.h"

SMMIterator::SMMIterator(const SortedMultiMap& d) : map(d) {
	if (map.head == -1) {
		currentKey = -1;
		currentKeyIndex = -1;
		return;
	}
	currentKey = map.head;
	currentKeyIndex = map.keys[currentKey].head;
}

void SMMIterator::first() {
	if (map.head == -1) {
		currentKey = -1;
		currentKeyIndex = -1;
		return;
	}
	currentKey = map.head;
	currentKeyIndex = map.keys[currentKey].head;
}

void SMMIterator::next() {
	if (valid()) {
		if (map.keys[currentKey].values[currentKeyIndex].next != -1) {
			currentKeyIndex = map.keys[currentKey].values[currentKeyIndex].next;
		}
		else {
			currentKey = map.keys[currentKey].next;
			if (currentKey == -1) {
				currentKeyIndex = -1;
				return;
			}
			currentKeyIndex = map.keys[currentKey].head;
		}
	}
}

bool SMMIterator::valid() const {
	return currentKey != -1;
}

TElem SMMIterator::getCurrent() const {
	return TElem(map.keys[currentKey].key, map.keys[currentKey].values[currentKeyIndex].value);
}


