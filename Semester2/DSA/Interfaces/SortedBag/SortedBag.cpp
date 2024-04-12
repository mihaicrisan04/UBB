#include "SortedBag.h"
#include "SortedBagIterator.h"
#include <stdexcept>

SortedBag::SortedBag(Relation r) : relation(r), head(nullptr), length(0) {}

void SortedBag::add(TComp e) {
	Node *current = head;
	Node *prev = nullptr;
	while (current != nullptr && relation(current->data.first, e)) {
		if (current->data.first == e) {
			current->data.second++;
			length++;
			return;
		}
		prev = current;
		current = current->next;
	}
	Node *newNode = new Node;
	newNode->data.first = e;
	newNode->data.second = 1;
	newNode->next = current;
	length++;
	if (prev == nullptr) {
		head = newNode;
	}
	else {
		prev->next = newNode;
	}
}


bool SortedBag::remove(TComp e) {
	Node *current = head;
	Node *prev = nullptr;
	while (current != nullptr && current->data.first != e) {
		prev = current;
		current = current->next;
	}
	if (current != nullptr) {
		if (current->data.second > 1) {
			current->data.second--;
		}
		else {
			if (prev == nullptr) {
				head = current->next;
			}
			else {
				prev->next = current->next;
			}
			delete current;
		}
		length--;
		return true;
	}
	return false;
}


bool SortedBag::search(TComp elem) const {
	Node *current = head;
	while (current != nullptr && current->data.first != elem) {
		current = current->next;
	}
	if (current != nullptr) {
		return true;
	}
	return false;
}


int SortedBag::nrOccurrences(TComp elem) const {
	Node *current = head;
	while (current != nullptr && current->data.first != elem) {
		current = current->next;
	}
	if (current != nullptr) {
		return current->data.second;
	}
	return 0;
}



int SortedBag::size() const {
	return length;
}


bool SortedBag::isEmpty() const {
	return head == nullptr;
}


SortedBagIterator SortedBag::iterator() const {
	return SortedBagIterator(*this);
}


SortedBag::~SortedBag() {
	Node *current = head;
	while (current != nullptr) {
		Node *next = current->next;
		delete current;
		current = next;
	}
}


int SortedBag::removeOccurences(int nr, TComp elem) {
	if (nr < 0) {
		throw std::invalid_argument("Number of occurences must be positive");
	}
	Node *current = head;
	Node *prev = nullptr;
	while (current != nullptr && current->data.first != elem) {
		prev = current;
		current = current->next;
	}
	if (current != nullptr) {
		if (current->data.second > nr) {
			current->data.second -= nr;
		}
		else {
			if (prev == nullptr) {
				head = current->next;
			}
			else {
				prev->next = current->next;
			}
			nr = current->data.second;
			delete current;
		}
		length -= nr;
		return nr;
	}
	return 0;
}
