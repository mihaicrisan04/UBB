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
	// complexity: theta(1)
	int nrLines() const;

	//returns the number of columns
	// complexity: theta(1)
	int nrColumns() const;

	//returns the element from line i and column j (indexing starts from 0)
	//throws exception if (i,j) is not a valid position in the Matrix
	// complexitiy: best O(1), worst O(log size)
	TElem element(int i, int j) const;

	//modifies the value from line i and column j
	//returns the previous value from the position
	//throws exception if (i,j) is not a valid position in the Matrix
	// complexity: best O(1), worst O(size) 
	TElem modify(int i, int j, TElem e);


	// returns the position of the element e from the Matrix (as a pair of lines and columns)
	// if the element occurs multiple times, return the position of the first occurrence
	// if the element is not in the Matrix, return a pair of -1, -1
	// comlexity: best O(1), worst O(size)
	std::pair<int, int> positionOf(TElem e) const;

	//destructor
	~Matrix();
};
