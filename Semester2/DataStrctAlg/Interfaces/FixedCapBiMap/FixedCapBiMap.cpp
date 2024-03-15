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
			for (int j = i + 1; j < sizeOfMap; j++) {
				if (elements[j].first == c) {
					return false;
				}
			}
		}
	}
	elements[sizeOfMap] = ValuePair(c, v); 
	sizeOfMap++;
	return true;	
}

ValuePair FixedCapBiMap::search(TKey c) const{
	ValuePair result = {NULL_TVALUE, NULL_TVALUE};
	for (int i = 0; i < sizeOfMap; i++) {

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



