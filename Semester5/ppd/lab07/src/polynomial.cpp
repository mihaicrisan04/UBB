#include "polynomial.hpp"
#include <algorithm>
#include <sstream>

Polynomial::Polynomial() : coeffs_{0} {}

Polynomial::Polynomial(std::vector<long> coeffs) : coeffs_(std::move(coeffs)) {
    if (coeffs_.empty()) {
        coeffs_ = {0};
    }
    trim();
}

Polynomial::Polynomial(const long* data, size_t size) {
    if (size == 0) {
        coeffs_ = {0};
    } else {
        coeffs_.assign(data, data + size);
        trim();
    }
}

void Polynomial::trim() {
    while (coeffs_.size() > 1 && coeffs_.back() == 0) {
        coeffs_.pop_back();
    }
}

int Polynomial::degree() const {
    return static_cast<int>(coeffs_.size()) - 1;
}

size_t Polynomial::size() const {
    return coeffs_.size();
}

long Polynomial::getCoeff(size_t idx) const {
    return idx < coeffs_.size() ? coeffs_[idx] : 0;
}

const std::vector<long>& Polynomial::coeffs() const {
    return coeffs_;
}

long* Polynomial::data() {
    return coeffs_.data();
}

const long* Polynomial::data() const {
    return coeffs_.data();
}

Polynomial Polynomial::random(size_t size, long min, long max, std::mt19937& rng) {
    if (size == 0) size = 1;
    std::uniform_int_distribution<long> dist(min, max);
    std::vector<long> c(size);
    for (auto& x : c) {
        x = dist(rng);
    }
    return Polynomial(std::move(c));
}

Polynomial Polynomial::allOnes(size_t size) {
    if (size == 0) size = 1;
    return Polynomial(std::vector<long>(size, 1));
}

Polynomial Polynomial::add(const Polynomial& a, const Polynomial& b) {
    size_t maxSz = std::max(a.size(), b.size());
    std::vector<long> result(maxSz);
    for (size_t i = 0; i < maxSz; ++i) {
        result[i] = a.getCoeff(i) + b.getCoeff(i);
    }
    return Polynomial(std::move(result));
}

Polynomial Polynomial::subtract(const Polynomial& a, const Polynomial& b) {
    size_t maxSz = std::max(a.size(), b.size());
    std::vector<long> result(maxSz);
    for (size_t i = 0; i < maxSz; ++i) {
        result[i] = a.getCoeff(i) - b.getCoeff(i);
    }
    return Polynomial(std::move(result));
}

bool Polynomial::operator==(const Polynomial& other) const {
    return coeffs_ == other.coeffs_;
}

bool Polynomial::operator!=(const Polynomial& other) const {
    return !(*this == other);
}

std::string Polynomial::toString(int maxShow) const {
    std::ostringstream oss;
    oss << "[";
    size_t n = coeffs_.size();
    if (n <= static_cast<size_t>(2 * maxShow)) {
        for (size_t i = 0; i < n; ++i) {
            if (i > 0) oss << ", ";
            oss << coeffs_[i];
        }
    } else {
        for (int i = 0; i < maxShow; ++i) {
            if (i > 0) oss << ", ";
            oss << coeffs_[i];
        }
        oss << ", ... (" << (n - 2 * maxShow) << " more), ";
        for (size_t i = n - maxShow; i < n; ++i) {
            if (i > n - maxShow) oss << ", ";
            oss << coeffs_[i];
        }
    }
    oss << "]";
    return oss.str();
}
