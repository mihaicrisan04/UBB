#pragma once

#include <vector>
#include <random>
#include <string>

class Polynomial {
public:
    Polynomial();
    explicit Polynomial(std::vector<long> coeffs);
    Polynomial(const long* data, size_t size);

    int degree() const;
    size_t size() const;
    long getCoeff(size_t idx) const;
    const std::vector<long>& coeffs() const;
    long* data();
    const long* data() const;

    static Polynomial random(size_t size, long min, long max, std::mt19937& rng);
    static Polynomial allOnes(size_t size);
    static Polynomial add(const Polynomial& a, const Polynomial& b);
    static Polynomial subtract(const Polynomial& a, const Polynomial& b);

    bool operator==(const Polynomial& other) const;
    bool operator!=(const Polynomial& other) const;

    std::string toString(int maxShow = 10) const;

private:
    std::vector<long> coeffs_;
    void trim();
};
