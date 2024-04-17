#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>
#include <vector>
#include <exception>
using namespace std;

SortedMultiMap::SortedMultiMap(Relation r) : relation(r), capacity(10), length(0), head(0), tail(0), firstEmpty(1) {
	keys = new KeyNode[capacity];
}

void SortedMultiMap::keysResize() {
	KeyNode* newKeys = new KeyNode[capacity * 2];
	for (int i = 0; i < capacity; i++) {
		newKeys[i] = keys[i];
	}
	delete[] keys;
	keys = newKeys;
	capacity *= 2;
}

void SortedMultiMap::valuesResize(int keyIndex) {
	ValueNode* newValues = new ValueNode[keys[keyIndex].valuesCapacity * 2];
	for (int i = 0; i < keys[keyIndex].valuesCapacity; i++) {
		newValues[i] = keys[keyIndex].values[i];
	}
	delete[] keys[keyIndex].values;
	keys[keyIndex].values = newValues;
	keys[keyIndex].valuesCapacity *= 2;
}

void SortedMultiMap::add(TKey c, TValue v) {
	if (length == 0) {

		return;
	}

	int keyIndex = head;
	while (keyIndex != 0 && !relation(keys[keyIndex].key, c)) {
		keyIndex = keys[keyIndex].next;
	}

	if (keyIndex == 0) {

	}
	else {
		
	}

}

vector<TValue> SortedMultiMap::search(TKey c) const {
	//TODO - Implementation
	return vector<TValue>();
}

bool SortedMultiMap::remove(TKey c, TValue v) {
	//TODO - Implementation
    return false;
}


int SortedMultiMap::size() const {
	return length;
}

bool SortedMultiMap::isEmpty() const {
	return length == 0;
}

SMMIterator SortedMultiMap::iterator() const {
	return SMMIterator(*this);
}

SortedMultiMap::~SortedMultiMap() {
	for (int i = 0; i < length; i++) {
		delete[] keys[i].values;
	}
	delete[] keys;
}
