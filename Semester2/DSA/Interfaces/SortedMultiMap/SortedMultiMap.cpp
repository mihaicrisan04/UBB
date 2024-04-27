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
	int i = head;
	while (i != -1) {
		newKeys[i] = keys[i];
		i = keys[i].next;
	}
	delete[] keys;
	keys = newKeys;
	capacity *= 2;
}

void SortedMultiMap::valuesResize(int keyIndex) {
	ValueNode* newValues = new ValueNode[keys[keyIndex].valuesCapacity * 2];
	int i = keys[keyIndex].head;
	while (i != -1) {
		newValues[i] = keys[keyIndex].values[i];
		i = keys[keyIndex].values[i].next;
	}
	delete[] keys[keyIndex].values;
	keys[keyIndex].values = newValues;
	keys[keyIndex].valuesCapacity *= 2;
}

//Complexity: O(k) - k is the number of keys 
// best case: Theta(1) - empty map
// worst case: Theta(k) - key is in the middle
void SortedMultiMap::add(TKey c, TValue v) {
	if (head == -1) {
		// empty map
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
		keys[firstEmpty].prev = -1;
		head = firstEmpty;
		tail = firstEmpty;
		firstEmpty++;
		length++;
		return;
	}

	int currentKey = head;
	while (currentKey != -1 && relation(keys[currentKey].key, c) && keys[currentKey].key != c) {
		cout << "prev " << keys[currentKey].prev << " current " << keys[currentKey].key << " next: " << keys[currentKey].next << "\n";
		currentKey = keys[currentKey].next;
	}

	if (keys[currentKey].key != c) {
		// new key
		if (firstEmpty == capacity) {
			keysResize();
		}

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

		if (currentKey == head) {
			// new head
			keys[firstEmpty].next = head;
			keys[firstEmpty].prev = -1;
			keys[head].prev = firstEmpty;
			head = firstEmpty;
		}
		else if (currentKey == -1) {
			// new tail
			keys[firstEmpty].next = -1;
			keys[firstEmpty].prev = tail;
			keys[tail].next = firstEmpty;
			tail = firstEmpty;
		}
		else {
			// new key in the middle add before currentKey
			// prev <-> firstEmpty <-> currentKey
			keys[firstEmpty].next = currentKey;  // firstEmpty -> currentKey
			keys[currentKey].prev = firstEmpty;  // firstEmpty <- currentKey 
			keys[firstEmpty].prev = keys[currentKey].prev;  // prev <- firstEmpty 
			keys[keys[currentKey].prev].next = firstEmpty;   // prev -> firstEmpty
		}
		firstEmpty++;
		length++;
	}
	else {
		// key already exists
		if (keys[currentKey].firstEmpty == keys[currentKey].valuesCapacity - 1) {
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
	}
}

// Complexity: O(k + n) - k is the number of keys, n is the number of values
// best case: Theta(1) - key is head or tail, and value is head or tail
// worst case: Theta(k + n) - key is in the middle, and value is in the middle
vector<TValue> SortedMultiMap::search(TKey c) const {
	vector<TValue> result;
	int currentKey = head;
	while (currentKey != -1 && relation(keys[currentKey].key, c) && keys[currentKey].key != c) {
		currentKey = keys[currentKey].next;
	}

	if (currentKey == -1 || keys[currentKey].key != c) {
		return result;
	}

	if (keys[currentKey].key == c) {
		// key found
		int currentValue = keys[currentKey].head;
		while (currentValue != -1) {
			result.push_back(keys[currentKey].values[currentValue].value);
			currentValue = keys[currentKey].values[currentValue].next;
		}
	}
	return result;
}

// Complexity: O(k + n) - k is the number of keys, n is the number of values
// best case: Theta(1) - key is head or tail, and value is head or tail
// worst case: Theta(k + n) - key is in the middle, and value is in the middle
bool SortedMultiMap::remove(TKey c, TValue v) {
	int currentKey = head;
	while (currentKey != -1 && relation(keys[currentKey].key, c) && keys[currentKey].key != c) {
		cout <<"prev " << keys[currentKey].prev << " current " << keys[currentKey].key << " next: " << keys[currentKey].next << "\n";
		currentKey = keys[currentKey].next;
	}
	

	cout << "In remove currentKey: " << keys[currentKey].key << " c: " << c << " map size: " << size() << "\n";
	if (currentKey == -1 || keys[currentKey].key != c) {
		return false;
	}

	if (keys[currentKey].key == c) {
		// key found
		int currentValue = keys[currentKey].head;
		while (currentValue != -1 && keys[currentKey].values[currentValue].value != v) {
			currentValue = keys[currentKey].values[currentValue].next;
		}

		if (currentValue == -1) {
			return false;
		}

		// cout << "In remove currentKey: " << keys[currentKey].key << " currentValue: " << keys[currentKey].values[currentValue].value << "\n";

		// value found
		if (keys[currentKey].valuesLength == 1) {
			// key has only one value -> remove key
			if (keys[currentKey].prev == -1) {
				// key is head
				int copyHead = head;
				head = keys[currentKey].next;
				if (head != -1) {
					keys[head].prev = -1;
				}
				delete keys[copyHead].values;
			}
			else if (keys[currentKey].next == -1) {
				// key is tail
				int copyTail = tail;
				tail = keys[currentKey].prev;
				if (tail != -1) {
					keys[tail].next = -1;
				}
				delete keys[copyTail].values;
			}
			else {
				// key is not head or tail
				keys[keys[currentKey].prev].next = keys[currentKey].next;
				keys[keys[currentKey].next].prev = keys[currentKey].prev;
				delete keys[currentKey].values;
			}
			keys[currentKey].valuesLength = 0;
			length--;
			return true;
		}

		// key has more than one value
		if (keys[currentKey].values[currentValue].prev == -1 and keys[currentKey].values[currentValue].next != -1) {
			// remove head
			keys[currentKey].head = keys[currentKey].values[currentValue].next;	
			keys[keys[currentKey].values[currentValue].next].prev = -1;
		}
		else if (keys[currentKey].values[currentValue].next == -1 and keys[currentKey].values[currentValue].prev != -1) {
			// remove tail
			cout << "values head: " << keys[currentKey].head << "\n";
			cout << "values tail: " << keys[currentKey].tail << "\n";
			cout << "prev tail: " << keys[currentKey].values[currentValue].prev << "\n";
			cout << "valuesLength: " << keys[currentKey].valuesLength << "\n";
			keys[currentKey].tail = keys[currentKey].values[currentValue].prev;
			keys[keys[currentKey].tail].next = -1;
		}
		else if (keys[currentKey].values[currentValue].prev != -1 && keys[currentKey].values[currentValue].next != -1) {
			// remove value in the middle
			keys[currentKey].values[keys[currentKey].values[currentValue].prev].next = keys[currentKey].values[currentValue].next;
			keys[currentKey].values[keys[currentKey].values[currentValue].next].prev = keys[currentKey].values[currentValue].prev;
		}
		keys[currentKey].valuesLength--;
		length--;
		return true;
	}
}

// Complexity: Theta(1)
int SortedMultiMap::size() const {
	return length;
}

// Complexity: Theta(1)
bool SortedMultiMap::isEmpty() const {
	return length == 0;
}

SMMIterator SortedMultiMap::iterator() const {
	return SMMIterator(*this);
}

void SortedMultiMap::filter(Condition c) {
	int currentKey = head;
	while (currentKey != -1) {
		int nextKey = keys[currentKey].next;

		int currentValue = keys[currentKey].head;
		while (currentValue != -1) {
			
			cout << "currentKey: " << keys[currentKey].key << " currentValue: " << keys[currentKey].values[currentValue].value << "\n";

			int nextValue = keys[currentKey].values[currentValue].next;

			if (!c(keys[currentKey].values[currentValue].value)) {
				remove(keys[currentKey].key, keys[currentKey].values[currentValue].value);	
			}

			currentValue = nextValue;
		}

		currentKey = nextKey;
	}
}

SortedMultiMap::~SortedMultiMap() {
	int currentKey = head;
	while (currentKey != -1) {
		delete[] keys[currentKey].values;
		currentKey = keys[currentKey].next;
	}
	delete[] keys;
}
