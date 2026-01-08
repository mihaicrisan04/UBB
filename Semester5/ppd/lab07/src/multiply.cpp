#include "multiply.hpp"
#include <algorithm>
#include <cmath>

namespace multiply {

Polynomial naiveSeq(const Polynomial& a, const Polynomial& b) {
    size_t n = a.size();
    size_t m = b.size();
    if (n == 0 || m == 0) {
        return Polynomial({0});
    }

    std::vector<long> result(n + m - 1, 0);
    for (size_t i = 0; i < n; ++i) {
        long ai = a.getCoeff(i);
        for (size_t j = 0; j < m; ++j) {
            result[i + j] += ai * b.getCoeff(j);
        }
    }
    return Polynomial(std::move(result));
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
    std::vector<long> result(n + m - 1, 0);
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = 0; j < m; ++j) {
            result[i + j] += a[i] * b[j];
        }
    }
    return result;
}

std::vector<long> karatsubaRec(const std::vector<long>& a, const std::vector<long>& b, int seqBase) {
    size_t n = a.size();

    if (n <= static_cast<size_t>(seqBase)) {
        return naiveSmall(a, b);
    }

    size_t mid = n / 2;

    std::vector<long> a0(a.begin(), a.begin() + mid);
    std::vector<long> a1(a.begin() + mid, a.end());
    std::vector<long> b0(b.begin(), b.begin() + mid);
    std::vector<long> b1(b.begin() + mid, b.end());

    auto z0 = karatsubaRec(a0, b0, seqBase);
    auto z2 = karatsubaRec(a1, b1, seqBase);

    auto a01 = addArrays(a0, a1);
    auto b01 = addArrays(b0, b1);
    auto z1Full = karatsubaRec(a01, b01, seqBase);
    auto z1 = subArrays(subArrays(z1Full, z0), z2);

    std::vector<long> result(2 * n - 1, 0);

    for (size_t i = 0; i < z0.size(); ++i) {
        result[i] += z0[i];
    }
    for (size_t i = 0; i < z1.size(); ++i) {
        result[i + mid] += z1[i];
    }
    for (size_t i = 0; i < z2.size(); ++i) {
        result[i + 2 * mid] += z2[i];
    }

    return result;
}

} // anonymous namespace

Polynomial karatsubaSeq(const Polynomial& a, const Polynomial& b, int seqBase) {
    size_t maxSize = std::max(a.size(), b.size());
    size_t paddedSize = 1;
    while (paddedSize < maxSize) {
        paddedSize <<= 1;
    }

    std::vector<long> aPad(paddedSize, 0);
    std::vector<long> bPad(paddedSize, 0);
    for (size_t i = 0; i < a.size(); ++i) aPad[i] = a.getCoeff(i);
    for (size_t i = 0; i < b.size(); ++i) bPad[i] = b.getCoeff(i);

    auto result = karatsubaRec(aPad, bPad, seqBase);

    size_t correctSize = a.size() + b.size() - 1;
    result.resize(correctSize);
    return Polynomial(std::move(result));
}

} // namespace multiply
