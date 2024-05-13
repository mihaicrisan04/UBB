#include "Set.h"
#include "SetIterator.h"


Set::Set() {
	capacity = 3000;
	list = new Node*[capacity];
	for (int i = 0; i < capacity; i++) {
		list[i] = nullptr;
	}
	length = 0;
}

void Set::resize() {
    int oldCapacity = capacity;
    Node** oldList = list;

    capacity *= 2;
    list = new Node*[capacity];

    for (int i = 0; i < capacity; i++) {
        list[i] = nullptr;
    }

    for (int i = 0; i < oldCapacity; i++) {
        Node* node = oldList[i];
        while (node != nullptr) {
            Node* nextNode = node->next;

            int newIndex = hash(node->data);
            node->next = list[newIndex];
            list[newIndex] = node;

            node = nextNode;
        }
    }

    delete[] oldList;
}

int Set::hash(TElem elem) const {
	int hash = elem % capacity;
	if (hash < 0) {
		hash += capacity;
	}
	return hash;
}


// Complexity: O(n) - n = the number of elements in the chain
// Best case: Θ(1) - the element is the first in the chain or the chain is empty
// Worst case: Θ(n) - the element is the last in the chain
// Average case: Θ(n)
bool Set::add(TElem elem) {
	int index = hash(elem);

	if (list[index] != nullptr) {
		Node* node = list[index];
		while (node != nullptr) {
			if (node->data == elem) return false;
			node = node->next;
		}
		Node* newNode = new Node;
		newNode->data = elem;	
		newNode->next = list[index];
		list[index] = newNode;
		length++;
		return true;
	}

	if (length == capacity) {
		resize();
	}

	Node* newNode = new Node;
	newNode->data = elem;
	newNode->next = nullptr;
	list[index] = newNode;
	length++;
	return true;
}


// Complexity: O(n) - n = the number of elements in the chain
// Best case: Θ(1) - the element is the first in the chain or the chain is empty
// Worst case: Θ(n) - the element is the last in the chain
// Average case: Θ(n)
bool Set::remove(TElem elem) {
	int index = hash(elem);

	if (list[index] != nullptr) {
		Node* node = list[index];
		Node* prevNode = nullptr;
		while (node != nullptr) {
			if (node->data == elem) {
				if (prevNode == nullptr) {
					list[index] = node->next;
				}
				else {
					prevNode->next = node->next;
				}
				delete node;
				length--;
				return true;
			}
			prevNode = node;
			node = node->next;
		}
	}

	return false;
}

// Complexity: O(n) - n = the number of elements in the chain
// Best case: Θ(1) - the element is the first in the chain or the chain is empty
// Worst case: Θ(n) - the element is the last in the chain
// Average case: Θ(n)
bool Set::search(TElem elem) const {
	int index = hash(elem);

	if (list[index] != nullptr) {
		Node* node = list[index];
		while (node != nullptr) {
			if (node->data == elem) return true;
			node = node->next;
		}
	}

	return false;
}


// Complexity: Θ(1)
int Set::size() const {
	return length;
}


// Complexity: Θ(1)
bool Set::isEmpty() const {
	return length == 0;
}


Set::~Set() {
	for (int i = 0; i < capacity; i++) {
		Node* node = list[i];
		while (node != nullptr) {
			Node* nextNode = node->next;
			delete node;
			node = nextNode;
		}
	}
}


SetIterator Set::iterator() const {
	return SetIterator(*this);
}


