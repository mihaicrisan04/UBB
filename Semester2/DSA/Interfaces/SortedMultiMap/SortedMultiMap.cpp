#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>
#include <vector>
#include <exception>
using namespace std;

SortedMultiMap::SortedMultiMap(Relation r) : relation(r), capacity(10), length(0), head(-1), tail(-1), firstEmpty(0) {
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
	if (firstEmpty == capacity) {
		keysResize();
	}

	if (head == -1) {
		head = 0;
		tail = 0;
		keys[0].key = c;
		keys[0].values = new ValueNode[10];
		keys[0].valuesCapacity = 10;
		keys[0].valuesLength = 1;
		keys[0].head = 0;
		keys[0].tail = 0;
		keys[0].firstEmpty = 1;
		keys[0].values[0].value = v;
		keys[0].values[0].next = -1;
		keys[0].values[0].prev = -1;
		keys[0].next = -1;
		keys[0].prev = -1;
		length++;
		firstEmpty++;
		return;
	}	

	int currentKey = head;
	while (currentKey != -1 && !relation(keys[currentKey].key, c)) {
		currentKey = keys[currentKey].next;
	}

	if (currentKey == -1) {
		// new key
		keys[firstEmpty].key = c;
		keys[firstEmpty].values = new ValueNode[10];
		keys[firstEmpty].valuesCapacity = 10;
		keys[firstEmpty].valuesLength = 1;
		keys[firstEmpty].head = 0;
		keys[firstEmpty].tail = 0;
		keys[firstEmpty].firstEmpty = 1;
		keys[firstEmpty].values[0].value = v;
		keys[firstEmpty].values[0].next = -1;
		keys[firstEmpty].values[0].prev = -1;
		keys[firstEmpty].next = -1;
		keys[firstEmpty].prev = tail;
		keys[tail].next = firstEmpty;
		tail = firstEmpty;
		firstEmpty++;
		length++;
		return;
	}
	else {
		if (keys[currentKey].valuesLength == keys[currentKey].valuesCapacity) {
			valuesResize(currentKey);
		}
		keys[currentKey].values[keys[currentKey].firstEmpty].value = v;
		keys[currentKey].values[keys[currentKey].firstEmpty].next = -1;
		keys[currentKey].values[keys[currentKey].firstEmpty].prev = keys[currentKey].tail;
		keys[currentKey].values[keys[currentKey].tail].next = keys[currentKey].firstEmpty;
		keys[currentKey].tail = keys[currentKey].firstEmpty;
		keys[currentKey].valuesLength++;
		keys[currentKey].firstEmpty++;
		length++;
		return;
	}
}

vector<TValue> SortedMultiMap::search(TKey c) const {
	vector<TValue> result;
	int currentKey = head;
	while (currentKey != -1 && !relation(keys[currentKey].key, c)) {
		currentKey = keys[currentKey].next;
	}

	if (currentKey == -1) {
		return result;
	}

	int currentValue = keys[currentKey].head;
	while (currentValue != -1) {
		result.push_back(keys[currentKey].values[currentValue].value);
		currentValue = keys[currentKey].values[currentValue].next;
	}
	return result;
}

bool SortedMultiMap::remove(TKey c, TValue v) {
	int currentKey = head;
	while (currentKey != -1 && !relation(keys[currentKey].key, c)) {
		currentKey = keys[currentKey].next;
	}

	if (currentKey == -1) {
		return false;
	}

	int currentValue = keys[currentKey].head;
	while (currentValue != -1 && keys[currentKey].values[currentValue].value != v) {
		currentValue = keys[currentKey].values[currentValue].next;
	}
	if (currentValue == -1) {
		return false;
	}
	keys[currentKey].valuesLength--;
	length--;
	keys[currentKey].values[keys[currentValue].prev].next = keys[currentValue].next;
	keys[currentKey].values[keys[currentValue].next].prev = keys[currentValue].prev;
	return true;
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
