#pragma once

#include "polynomial.hpp"

namespace multiply {

Polynomial naiveSeq(const Polynomial& a, const Polynomial& b);

Polynomial karatsubaSeq(const Polynomial& a, const Polynomial& b, int seqBase = 64);

} // namespace multiply
