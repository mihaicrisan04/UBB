#pragma once

#include "SortedMultiMap.h"
#include <exception>


class SMMIterator{
	friend class SortedMultiMap;
private:
	//DO NOT CHANGE THIS PART
	const SortedMultiMap& map;
	SMMIterator(const SortedMultiMap& map);

	//TODO - Representation
	int currentKeyIndex;
	int currentValueIndex;	

public:
	void first();
	void next();
	bool valid() const;
   	TElem getCurrent() const;
};

