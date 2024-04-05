#include "SortedBagIterator.h"
#include "SortedBag.h"
#include <exception>

using namespace std;

SortedBagIterator::SortedBagIterator(const SortedBag& b) : bag(b), current(b.head), currentOccurences(0) {}

TComp SortedBagIterator::getCurrent() {
	if (!valid()) {
		throw exception();
	}
	return current->data.first;
}

bool SortedBagIterator::valid() {
	return current != nullptr;
}

void SortedBagIterator::next() {
	if (!valid()) {
		throw exception();
	}
	if (currentOccurences < current->data.second - 1) {
		currentOccurences++;
		return;
	}
	current = current->next;	
	currentOccurences = 0;
}

void SortedBagIterator::first() {
	current = bag.head;
}

