#pragma once

#include <vector>

//DO NOT CHANGE THIS PART
typedef int TElem;
typedef std::pair<std::pair<TElem, TElem>, TElem> Triple;
#define NULL_TELEM 0

class Matrix {

private:
	//TODO - Representation
	TElem _nrLines;
	TElem _nrCols;
	TElem capacity = 10;
	TElem size;
	Triple* elements;
public:
	//constructor
	Matrix(int nrLines, int nrCols);

	//returns the number of lines
	int nrLines() const;

	//returns the number of columns
	int nrColumns() const;

	//returns the element from line i and column j (indexing starts from 0)
	//throws exception if (i,j) is not a valid position in the Matrix
	TElem element(int i, int j) const;

	//modifies the value from line i and column j
	//returns the previous value from the position
	//throws exception if (i,j) is not a valid position in the Matrix
	TElem modify(int i, int j, TElem e);

	//destructor
	~Matrix();
};
