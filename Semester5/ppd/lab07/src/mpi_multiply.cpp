#include "mpi_multiply.hpp"
#include "multiply.hpp"
#include <algorithm>
#include <cmath>
#include <vector>

namespace mpi_multiply {

Polynomial naiveMPI(const Polynomial& a, const Polynomial& b, MPI_Comm comm) {
    int rank, numProcs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &numProcs);

    long sizeA, sizeB;
    std::vector<long> coeffsA, coeffsB;

    if (rank == 0) {
        sizeA = static_cast<long>(a.size());
        sizeB = static_cast<long>(b.size());
        coeffsA = a.coeffs();
        coeffsB = b.coeffs();
    }

    MPI_Bcast(&sizeA, 1, MPI_LONG, 0, comm);
    MPI_Bcast(&sizeB, 1, MPI_LONG, 0, comm);

    if (rank != 0) {
        coeffsA.resize(sizeA);
        coeffsB.resize(sizeB);
    }

    MPI_Bcast(coeffsA.data(), sizeA, MPI_LONG, 0, comm);
    MPI_Bcast(coeffsB.data(), sizeB, MPI_LONG, 0, comm);

    long resultSize = sizeA + sizeB - 1;

    long chunkSize = (resultSize + numProcs - 1) / numProcs;
    long start = rank * chunkSize;
    long end = std::min(start + chunkSize, resultSize);

    std::vector<long> localResult(chunkSize, 0);

    for (long k = start; k < end; ++k) {
        long iMin = std::max(0L, k - sizeB + 1);
        long iMax = std::min(k, sizeA - 1);
        long sum = 0;
        for (long i = iMin; i <= iMax; ++i) {
            sum += coeffsA[i] * coeffsB[k - i];
        }
        localResult[k - start] = sum;
    }

    std::vector<long> globalResult;
    if (rank == 0) {
        globalResult.resize(numProcs * chunkSize);
    }

    MPI_Gather(localResult.data(), chunkSize, MPI_LONG,
               globalResult.data(), chunkSize, MPI_LONG,
               0, comm);

    if (rank == 0) {
        globalResult.resize(resultSize);
        return Polynomial(std::move(globalResult));
    }

    return Polynomial();
}

namespace {

std::vector<long> addArrays(const std::vector<long>& a, const std::vector<long>& b) {
    size_t maxLen = std::max(a.size(), b.size());
    std::vector<long> result(maxLen);
    for (size_t i = 0; i < maxLen; ++i) {
        long ai = i < a.size() ? a[i] : 0;
        long bi = i < b.size() ? b[i] : 0;
        result[i] = ai + bi;
    }
    return result;
}

std::vector<long> subArrays(const std::vector<long>& a, const std::vector<long>& b) {
    size_t maxLen = std::max(a.size(), b.size());
    std::vector<long> result(maxLen);
    for (size_t i = 0; i < maxLen; ++i) {
        long ai = i < a.size() ? a[i] : 0;
        long bi = i < b.size() ? b[i] : 0;
        result[i] = ai - bi;
    }
    return result;
}

std::vector<long> naiveSmall(const std::vector<long>& a, const std::vector<long>& b) {
    size_t n = a.size();
    size_t m = b.size();
    if (n == 0 || m == 0) return {0};
    std::vector<long> result(n + m - 1, 0);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            result[i + j] += a[i] * b[j];
        }
    }
    return result;
}

std::vector<long> karatsubaRecLocal(const std::vector<long>& a, const std::vector<long>& b, int seqBase) {
    size_t n = a.size();
    if (n <= static_cast<size_t>(seqBase)) {
        return naiveSmall(a, b);
    }

    size_t mid = n / 2;

    std::vector<long> a0(a.begin(), a.begin() + mid);
    std::vector<long> a1(a.begin() + mid, a.end());
    std::vector<long> b0(b.begin(), b.begin() + mid);
    std::vector<long> b1(b.begin() + mid, b.end());

    auto z0 = karatsubaRecLocal(a0, b0, seqBase);
    auto z2 = karatsubaRecLocal(a1, b1, seqBase);

    auto a01 = addArrays(a0, a1);
    auto b01 = addArrays(b0, b1);
    auto z1Full = karatsubaRecLocal(a01, b01, seqBase);
    auto z1 = subArrays(subArrays(z1Full, z0), z2);

    std::vector<long> result(2 * n - 1, 0);
    for (size_t i = 0; i < z0.size(); ++i) result[i] += z0[i];
    for (size_t i = 0; i < z1.size(); ++i) result[i + mid] += z1[i];
    for (size_t i = 0; i < z2.size(); ++i) result[i + 2 * mid] += z2[i];

    return result;
}

} // anonymous namespace

Polynomial karatsubaMPI(const Polynomial& a, const Polynomial& b, MPI_Comm comm, int seqBase) {
    int rank, numProcs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &numProcs);

    long sizeA, sizeB;
    std::vector<long> coeffsA, coeffsB;

    if (rank == 0) {
        sizeA = static_cast<long>(a.size());
        sizeB = static_cast<long>(b.size());
    }

    MPI_Bcast(&sizeA, 1, MPI_LONG, 0, comm);
    MPI_Bcast(&sizeB, 1, MPI_LONG, 0, comm);

    size_t maxSize = std::max(static_cast<size_t>(sizeA), static_cast<size_t>(sizeB));
    size_t paddedSize = 1;
    while (paddedSize < maxSize) {
        paddedSize <<= 1;
    }

    std::vector<long> aPad(paddedSize, 0);
    std::vector<long> bPad(paddedSize, 0);

    if (rank == 0) {
        for (size_t i = 0; i < a.size(); ++i) aPad[i] = a.getCoeff(i);
        for (size_t i = 0; i < b.size(); ++i) bPad[i] = b.getCoeff(i);
    }

    MPI_Bcast(aPad.data(), paddedSize, MPI_LONG, 0, comm);
    MPI_Bcast(bPad.data(), paddedSize, MPI_LONG, 0, comm);

    size_t mid = paddedSize / 2;
    std::vector<long> a0(aPad.begin(), aPad.begin() + mid);
    std::vector<long> a1(aPad.begin() + mid, aPad.end());
    std::vector<long> b0(bPad.begin(), bPad.begin() + mid);
    std::vector<long> b1(bPad.begin() + mid, bPad.end());

    auto a01 = addArrays(a0, a1);
    auto b01 = addArrays(b0, b1);

    int assignment = rank % 3;
    std::vector<long> localZ;

    if (assignment == 0) {
        localZ = karatsubaRecLocal(a0, b0, seqBase);
    } else if (assignment == 1) {
        localZ = karatsubaRecLocal(a1, b1, seqBase);
    } else {
        localZ = karatsubaRecLocal(a01, b01, seqBase);
    }

    size_t z0Size = 2 * mid - 1;
    size_t z2Size = 2 * mid - 1;
    size_t z1FullSize = 2 * a01.size() - 1;

    std::vector<long> z0(z0Size, 0);
    std::vector<long> z2(z2Size, 0);
    std::vector<long> z1Full(z1FullSize, 0);

    if (numProcs >= 3) {
        int root0 = 0;
        int root1 = 1 % numProcs;
        int root2 = 2 % numProcs;

        if (assignment == 0) z0 = localZ;
        if (assignment == 1) z2 = localZ;
        if (assignment == 2) z1Full = localZ;

        std::vector<long> z0Recv(z0Size, 0);
        std::vector<long> z2Recv(z2Size, 0);
        std::vector<long> z1FullRecv(z1FullSize, 0);

        if (rank == root0 && assignment == 0) {
            z0Recv = z0;
        }
        MPI_Bcast(z0Recv.data(), z0Size, MPI_LONG, root0, comm);

        if (rank == root1 && assignment == 1) {
            z2Recv = z2;
        }
        MPI_Bcast(z2Recv.data(), z2Size, MPI_LONG, root1, comm);

        if (rank == root2 && assignment == 2) {
            z1FullRecv = z1Full;
        }
        MPI_Bcast(z1FullRecv.data(), z1FullSize, MPI_LONG, root2, comm);

        z0 = z0Recv;
        z2 = z2Recv;
        z1Full = z1FullRecv;
    } else {
        z0 = karatsubaRecLocal(a0, b0, seqBase);
        z2 = karatsubaRecLocal(a1, b1, seqBase);
        z1Full = karatsubaRecLocal(a01, b01, seqBase);
    }

    auto z1 = subArrays(subArrays(z1Full, z0), z2);

    std::vector<long> result(2 * paddedSize - 1, 0);
    for (size_t i = 0; i < z0.size(); ++i) result[i] += z0[i];
    for (size_t i = 0; i < z1.size(); ++i) result[i + mid] += z1[i];
    for (size_t i = 0; i < z2.size(); ++i) result[i + 2 * mid] += z2[i];

    size_t correctSize = sizeA + sizeB - 1;
    result.resize(correctSize);

    if (rank == 0) {
        return Polynomial(std::move(result));
    }
    return Polynomial();
}

} // namespace mpi_multiply
