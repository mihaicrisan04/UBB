#include "SetIterator.h"
#include "Set.h"


SetIterator::SetIterator(const Set& m) : set(m) {
	first();
}


void SetIterator::first() {
	index = 0;
	while (index < set.capacity && set.list[index] == nullptr) {
		index++;
	}
	if (index < set.capacity) {
		node = set.list[index];
	}
	else {
		node = nullptr;
	}
}


void SetIterator::next() {
	if (!valid()) {
		throw std::exception();
	}

	node = node->next;
	if (node == nullptr) {
		index++;
		while (index < set.capacity && set.list[index] == nullptr) {
			index++;
		}
		if (index < set.capacity) {
			node = set.list[index];
		}
	}
}


TElem SetIterator::getCurrent() {
	if (!valid()) {
		throw std::exception();
	}
	return node->data;
}

bool SetIterator::valid() const {
	return index < set.capacity && set.list[index] != nullptr;
}



