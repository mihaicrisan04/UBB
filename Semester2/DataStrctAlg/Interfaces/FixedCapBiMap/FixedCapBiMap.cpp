#include "FixedCapBiMap.h"
#include "FixedCapBiMapIterator.h"
#include <exception>


FixedCapBiMap::FixedCapBiMap(int capacity) : capacity(capacity), sizeOfMap(0), elements(new TElem[capacity]) {  
	if (capacity <= 0) {
		throw std::exception();
	}
}

FixedCapBiMap::~FixedCapBiMap() {
	delete[] elements;
}

bool FixedCapBiMap::add(TKey c, TValue v){
	if (sizeOfMap == capacity) {
		throw std::exception();
	}
	for (int i = 0; i < sizeOfMap; i++) {
		if (elements[i].first == c) {
			if (elements[i].second.first != NULL_TVALUE && elements[i].second.second != NULL_TVALUE) {
				return false;
			}
			if (elements[i].second.first == NULL_TVALUE) {
				elements[i].second.first = v;
				return true;
			}
			if (elements[i].second.second == NULL_TVALUE) {
				elements[i].second.second = v;
				return true;
			}
		}
	}
	elements[sizeOfMap] = std::pair<TKey, ValuePair>(c, std::pair<TValue, TValue>(v, NULL_TVALUE));
	sizeOfMap++;
	return true;	
}

ValuePair FixedCapBiMap::search(TKey c) const{
	ValuePair result = std::pair<TValue, TValue>(NULL_TVALUE, NULL_TVALUE);
	for (int i = 0; i < sizeOfMap; i++) {
		if (elements[i].first == c) {
			if (elements[i].second.first != NULL_TVALUE) {
				result.first = elements[i].second.first;
			}
			if (elements[i].second.second != NULL_TVALUE) {
				result.second = elements[i].second.second;
			}
			return result;
		}
	}
}

bool FixedCapBiMap::remove(TKey c, TValue v){
	for (int i = 0; i < sizeOfMap; i++) {
		if (elements[i].first == c) {
			if (elements[i].second.first == v) {
				elements[i].second.first = elements[i].second.second;
				elements[i].second.second = NULL_TVALUE;
				return true;
			}
			if (elements[i].second.second == v) {
				elements[i].second.second = NULL_TVALUE;
				return true;
			}	
		}
	}
	return false;
}


int FixedCapBiMap::size() const {
	return sizeOfMap;
}

bool FixedCapBiMap::isEmpty() const{
	return sizeOfMap == 0;
}

bool FixedCapBiMap::isFull() const {
	return sizeOfMap == capacity;
}

FixedCapBiMapIterator FixedCapBiMap::iterator() const {
	return FixedCapBiMapIterator(*this);
}



