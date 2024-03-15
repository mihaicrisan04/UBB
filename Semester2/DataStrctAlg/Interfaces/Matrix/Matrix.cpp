#include "Matrix.h"
#include <exception>
using namespace std;


Matrix::Matrix(int nrLines, int nrCols) : _nrLines(nrLines), _nrCols(nrCols), elements(new Triple[capacity]), size(0) {
	//TODO - Implementation
}


int Matrix::nrLines() const {
	//TODO - Implementation
	return _nrLines;
}


int Matrix::nrColumns() const {
	//TODO - Implementation
	return _nrCols;
}


TElem Matrix::element(int i, int j) const {
	//TODO - Implementation
	if (i < 0 || i >= _nrLines || j < 0 || j >= _nrCols) {
		throw exception();
	}
	for (int k = 0; k < size; k++) {
		if (elements[k].first.first == i && elements[k].first.second == j) {
			return elements[k].second;
		}
	}
	return NULL_TELEM;
}

TElem Matrix::modify(int i, int j, TElem e) {
	//TODO - Implementation
	if (i < 0 || i >= _nrLines || j < 0 || j >= _nrCols) {
		throw exception();
	}
	for (int k = 0; k < size; k++) {
		if (elements[k].first.first == i && elements[k].first.second == j) {
			TElem old = elements[k].second;
			if (e == NULL_TELEM) {
				for (int l = k; l < size - 1; l++) {
					elements[l] = elements[l + 1];
				}
				size--;
			}
			else {
				elements[k].second = e;
			}
			return old;
		}
	}
	// add the new triple at the right position
	if (e != NULL_TELEM) {
		if (size == capacity) {
			capacity *= 2;
			Triple* newElements = new Triple[capacity];
			for (int k = 0; k < size; k++) {
				newElements[k] = elements[k];
			}
			delete[] elements;
			elements = newElements;
		}
		int pos = 0;
		while (pos < size && elements[pos].first.first < i) {
			pos++;
		}
		while (pos < size && elements[pos].first.first == i && elements[pos].first.second < j) {
			pos++;
		}
		for (int k = size; k > pos; k--) {
			elements[k] = elements[k - 1];
		}
		elements[pos] = {{i, j}, e};
		size++;
	}
	return NULL_TELEM;
}


Matrix::~Matrix() {
	//TODO - Implementation
	delete[] elements;
}


