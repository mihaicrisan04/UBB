#include "ListIterator.h"
#include "SortedIndexedList.h"
#include <iostream>
using namespace std;
#include <exception>

SortedIndexedList::SortedIndexedList(Relation r) : r(r) {
	capacity = 10;
	nodes = new Node[capacity];
	freeStack = new int[capacity];

	root = NULL_TCOMP;
	length = 0;

	freeTop = 0;
	for (int i = 0; i < capacity; i++) {
		freeStack[freeTop++] = i;
	}
}

void SortedIndexedList::resize() {
	Node* newNodes = new Node[capacity * 2];
	int* newFreeStack = new int[capacity * 2];

	for (int i = 0; i < capacity; i++) {
		newNodes[i] = nodes[i];
		newFreeStack[i] = freeStack[i];
	}

	for (int i = capacity; i < capacity * 2; i++) {
		newFreeStack[freeTop++] = i;
	}

	delete[] nodes;
	delete[] freeStack;

	nodes = newNodes;
	freeStack = newFreeStack;
	capacity *= 2;
}

int SortedIndexedList::size() const {
	return length;
}

bool SortedIndexedList::isEmpty() const {
	return length == 0;
}

TComp SortedIndexedList::getElement(int i) const{
	if (i < 0 || i >= length) {
		throw exception();
	}
	int current = root;
	while (current != NULL_TCOMP) {
		if (nodes[current].leftCount == i) {
			return nodes[current].value;
		}
		else if (nodes[current].leftCount > i) {
			current = nodes[current].left;
		}
		else {
			i -= nodes[current].leftCount + 1;
			current = nodes[current].right;
		}
	}
}

TComp SortedIndexedList::remove(int i) {
	if (i < 0 || i >= length) {
		throw exception();
	}

	int current = root;
	int parent = NULL_TCOMP;
	while (current != NULL_TCOMP) {
		if (nodes[current].leftCount == i) {
			break;
		}
		else if (nodes[current].leftCount > i) {
			nodes[current].leftCount--;
			parent = current;
			current = nodes[current].left;
		}
		else {
			i -= nodes[current].leftCount + 1;
			parent = current;
			current = nodes[current].right;
		}
	}

	TComp removed = nodes[current].value;

	if (nodes[current].left == NULL_TCOMP && nodes[current].right != NULL_TCOMP) {
		transplant(current, nodes[current].right);
	}
	else if (nodes[current].right == NULL_TCOMP && nodes[current].left != NULL_TCOMP) {
		transplant(current, nodes[current].left);
	}
	else if (nodes[current].left != NULL_TCOMP && nodes[current].right != NULL_TCOMP) {
		int successor = nodes[current].right;
		while (nodes[successor].left != NULL_TCOMP) {
			successor = nodes[successor].left;
		}

		if (nodes[successor].parent != current) {
			transplant(successor, nodes[successor].right);
			nodes[successor].right = nodes[current].right;
			nodes[nodes[successor].right].parent = successor;
		}

		transplant(current, successor);
		nodes[successor].left = nodes[current].left;
		nodes[nodes[successor].left].parent = successor;
	}
	else {
		transplant(current, NULL_TCOMP);
	}

	freeStack[freeTop++] = current;
	length--;

	return removed;
}

void SortedIndexedList::transplant(int u, int v) {
	if (nodes[u].parent == NULL_TCOMP) {
		root = v;
	}
	else if (u == nodes[nodes[u].parent].left) {
		nodes[nodes[u].parent].left = v;
	}
	else {
		nodes[nodes[u].parent].right = v;
	}

	if (v != NULL_TCOMP) {
		nodes[v].parent = nodes[u].parent;
	}
}

int SortedIndexedList::search(TComp e) const {
	int current = root;
	while (current != NULL_TCOMP) {
		if (nodes[current].value == e) {
			return nodes[current].leftCount;
		}
		else if (r(e, nodes[current].value)) {
			current = nodes[current].left;
		}
		else {
			current = nodes[current].right;
		}
	}
	return -1;
}

void SortedIndexedList::add(TComp e) {
	if (length == capacity) {
		resize();
	}

	if (root == NULL_TCOMP) {
		root = freeStack[--freeTop];
		nodes[root].value = e;
		nodes[root].left = NULL_TCOMP;
		nodes[root].right = NULL_TCOMP;
		nodes[root].parent = NULL_TCOMP;
		nodes[root].leftCount = 0;
		length++;
		return;
	}

	int current = root;
	int parent = NULL_TCOMP;

	while (current != NULL_TCOMP) {
		parent = current;
		if (r(e, nodes[current].value)) {
			nodes[current].leftCount++;
			current = nodes[current].left;
		}
		else {
			current = nodes[current].right;
		}
	}

	int newNode = freeStack[--freeTop];
	nodes[newNode].value = e;
	nodes[newNode].left = NULL_TCOMP;
	nodes[newNode].right = NULL_TCOMP;
	nodes[newNode].parent = parent;
	nodes[newNode].leftCount = 0;

	(r(e, nodes[parent].value) ? nodes[parent].left : nodes[parent].right) = newNode;

	length++;
}

ListIterator SortedIndexedList::iterator(){
	return ListIterator(*this);
}

//destructor
SortedIndexedList::~SortedIndexedList() {
	delete[] nodes;
	delete[] freeStack;
}
