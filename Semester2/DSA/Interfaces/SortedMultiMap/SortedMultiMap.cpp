#include "SMMIterator.h"
#include "SortedMultiMap.h"
#include <iostream>
#include <vector>
#include <exception>
using namespace std;
SortedMultiMap::SortedMultiMap(Relation r) {
	capacity = 10;
	Size = 0;
	head = -1;
	tail = -1;
	firstEmpty = 0;
	keysCount = 0;
	this->r = r;
	arr = new Node[capacity];
	for (int i = 0; i < capacity - 1; i++) {
		arr[i].next = i + 1;
		arr[i].prev = i - 1;
	}
	arr[capacity - 1].next = -1;
}

void SortedMultiMap::resize() {
	Node* new_arr = new Node[2 * capacity];
	for (int i = 0; i < capacity; i++) {
		new_arr[i].key = arr[i].key;
		new_arr[i].values = arr[i].values.deepCopy();
		new_arr[i].next = arr[i].next;
		new_arr[i].prev = arr[i].prev;
	}
	for (int i = capacity; i < 2 * capacity; i++) {
		new_arr[i].next = i + 1;
		new_arr[i].prev = i - 1;
	}
	new_arr[2 * capacity - 1].next = -1;
	delete[] arr;
	arr = new_arr;
	firstEmpty = capacity;
	capacity *= 2;
}

void SortedMultiMap::add(TKey c, TValue v) {
	if (keysCount == capacity) {
		resize();
	}

	if (head == -1) {
		head = firstEmpty;
		tail = firstEmpty;
		firstEmpty = arr[firstEmpty].next;
		arr[head].key = c;
		arr[head].values.add(v);
		arr[head].next = -1;
		arr[head].prev = -1;
		keysCount++;
		Size++;
		return;
	}

	int current = head;
	while (current != -1 && r(arr[current].key, c) && arr[current].key != c) {
		current = arr[current].next;
	}


	if (current == -1) {
		// add at the end
		arr[tail].next = firstEmpty;
		arr[firstEmpty].prev = tail;
		tail = firstEmpty;
		firstEmpty = arr[firstEmpty].next;
		arr[tail].key = c;
		arr[tail].values.add(v);
		arr[tail].next = -1;
		Size++;
		keysCount++;
		return;
	}

	if (arr[current].key == c) {
		// add to the current key
		arr[current].values.add(v);
		Size++;
		return;
	}

	if (current == head) {
		// add at the beginning
		arr[head].prev = firstEmpty;
		head = firstEmpty;
		firstEmpty = arr[firstEmpty].next;
		arr[head].next = current;
		arr[head].key = c;
		arr[head].values.add(v);
		arr[head].prev = -1;
		Size++;
		keysCount++;
		return;
	}
	// add in the middle before current
	int newElem = firstEmpty;
	firstEmpty = arr[firstEmpty].next;
	arr[newElem].prev = arr[current].prev;
	arr[newElem].next = current;
	arr[arr[current].prev].next = newElem;
	arr[current].prev = newElem;
	arr[newElem].key = c;
	arr[newElem].values.add(v);
	Size++;
	keysCount++;
	return;
}

vector<TValue> SortedMultiMap::search(TKey c) const {
	int current = head;
	while (current != -1 && arr[current].key != c) {
		current = arr[current].next;
	}

	if (current == -1) {
		return vector<TValue>();
	}
	return arr[current].values.toVector();
}

bool SortedMultiMap::remove(TKey c, TValue v) {
	int current = head;
	while (current != -1 && arr[current].key != c) {
		current = arr[current].next;
	}

	if (current == -1) {
		return false;
	}

	if (arr[current].values.remove(v)) {
		Size--;
		if (arr[current].values.getSize() == 0) {
			if (current == head) {
				head = arr[current].next;
				if (head != -1) {
					arr[head].prev = -1;
				}
			}
			else if (current == tail) {
				tail = arr[current].prev;
				arr[tail].next = -1;
			}
			else {
				arr[arr[current].prev].next = arr[current].next;
				arr[arr[current].next].prev = arr[current].prev;
			}
			keysCount--;
			arr[current].next = firstEmpty;
			firstEmpty = current;
		}
		return true;
	}
	return false;
}


int SortedMultiMap::size() const {
	return Size;
}

bool SortedMultiMap::isEmpty() const {
	return Size == 0;
}

void SortedMultiMap::filter(Condition cond) {
	int current = head;
	while (current != -1) {
		vector<TValue> values = arr[current].values.toVector();	
		for (int i = 0; i < values.size(); i++) {
			if (!cond(values[i])) {
				remove(arr[current].key, values[i]);
			}
		}
		current = arr[current].next;
	}
}

SMMIterator SortedMultiMap::iterator() const {
	return SMMIterator(*this);
}

SortedMultiMap::~SortedMultiMap() {
	delete[] arr;
}
