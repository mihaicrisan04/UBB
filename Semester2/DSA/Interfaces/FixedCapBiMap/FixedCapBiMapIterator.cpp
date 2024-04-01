#include "FixedCapBiMap.h"
#include "FixedCapBiMapIterator.h"
#include <exception>
using namespace std;


FixedCapBiMapIterator::FixedCapBiMapIterator(const FixedCapBiMap& d) : map(d), currentPos(0) {

}


void FixedCapBiMapIterator::first() {
	currentPos = 0;	
}


void FixedCapBiMapIterator::next() {
	if (!valid()) {
		throw std::exception();
	}
	currentPos++;	
}


TElem FixedCapBiMapIterator::getCurrent(){
	if (!valid()) {
		throw std::exception();
	}
	return map.elements[currentPos];
}


bool FixedCapBiMapIterator::valid() const {
	return currentPos < map.sizeOfMap;
}



