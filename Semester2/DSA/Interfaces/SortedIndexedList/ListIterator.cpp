#include "ListIterator.h"
#include "SortedIndexedList.h"
#include <iostream>

using namespace std;

ListIterator::ListIterator(const SortedIndexedList& list) : list(list) {
	stack = new int[list.size() + 1];
	first();
}

void ListIterator::first() {
	int current = list.root;
	top = -1;
	while (current != NULL_TCOMP) {
		stack[++top] = current;
		current = list.nodes[current].left;
	}
}

void ListIterator::next() {
	int current = list.nodes[top--].right;
	cout << top << endl;
	while (current != NULL_TCOMP) {
		cout << current << endl;
		stack[++top] = current;
		current = list.nodes[current].left;
	}
}

bool ListIterator::valid() const{
	return top >= 0 && top < list.size();
}

TComp ListIterator::getCurrent() const{
	if (!valid()) {
		throw exception();
	}

	return list.nodes[stack[top]].value;
}


