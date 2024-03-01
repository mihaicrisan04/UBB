#include "Bag.h"
#include "BagIterator.h"
#include <exception>
#include <iostream>
#include <vector>

using namespace std;


Bag::Bag() {
	//TODO - Implementation
}


void Bag::add(TElem elem) {
	//TODO - Implementation
	array.push_back(elem);	
}


bool Bag::remove(TElem elem) {
	//TODO - Implementation
	for (int i = 0; i < array.size(); i++) {
		if (array[i] == elem) {
			array.erase(array.begin() + i);
			return true;
		}
	}
	return false; 
}


bool Bag::search(TElem elem) const {
	//TODO - Implementation
	for (int i = 0; i < array.size(); i++) {
		if (array[i] == elem) {
			return true;
		}
	}
	return false; 
}

int Bag::nrOccurrences(TElem elem) const {
	//TODO - Implementation
	int count = 0;
	for (int i = 0; i < array.size(); i++) {
		if (array[i] == elem) {
			count++;
		}
	}
	return count;
}


int Bag::size() const {
	//TODO - Implementation
	return array.size();
}


bool Bag::isEmpty() const {
	//TODO - Implementation
	return array.size() == 0;
}

BagIterator Bag::iterator() const {
	return BagIterator(*this);
}


Bag::~Bag() {
	//TODO - Implementation
}

