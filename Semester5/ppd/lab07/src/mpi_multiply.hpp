#pragma once

#include "polynomial.hpp"
#include <mpi.h>

namespace mpi_multiply {

Polynomial naiveMPI(const Polynomial& a, const Polynomial& b, MPI_Comm comm);

Polynomial karatsubaMPI(const Polynomial& a, const Polynomial& b, MPI_Comm comm, int seqBase = 64);

} // namespace mpi_multiply
