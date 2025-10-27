// File: scalar_product_cpp.cpp
#include <algorithm>
#include <chrono>
#include <condition_variable>
#include <deque>
#include <iostream>
#include <mutex>
#include <random>
#include <thread>
#include <vector>

struct BoundedQueue {
    std::deque<double> dq;
    size_t capacity;
    std::mutex mtx;
    std::condition_variable cvNotFull;
    std::condition_variable cvNotEmpty;
    bool done = false; // set by producer when finished

    explicit BoundedQueue(size_t cap) : capacity(cap) {}

    void put(double value) {
        std::unique_lock<std::mutex> lk(mtx);
        cvNotFull.wait(lk, [&]{ return dq.size() < capacity; });
        dq.push_back(value);
        cvNotEmpty.notify_one();
    }

    // Returns false if finished and queue empty
    bool take(double& out) {
        std::unique_lock<std::mutex> lk(mtx);
        cvNotEmpty.wait(lk, [&]{ return !dq.empty() || done; });
        if (dq.empty() && done) return false;
        out = dq.front();
        dq.pop_front();
        cvNotFull.notify_one();
        return true;
    }

    void set_done() {
        std::lock_guard<std::mutex> lk(mtx);
        done = true;
        cvNotEmpty.notify_all();
    }
};

int main(int argc, char** argv) {
    size_t n = 5'000'000;   // vector length
    size_t queueSize = 1024;
    if (argc >= 2) n = std::stoull(argv[1]);
    if (argc >= 3) queueSize = std::stoull(argv[2]);

    std::vector<double> a(n), b(n);
    std::mt19937_64 rng(42);
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    for (size_t i = 0; i < n; ++i) {
        a[i] = dist(rng);
        b[i] = dist(rng);
    }

    BoundedQueue buffer(queueSize);
    double result = 0.0;

    auto start = std::chrono::steady_clock::now();

    std::thread producer([&]{
        for (size_t i = 0; i < n; ++i) {
            buffer.put(a[i] * b[i]);
        }
        buffer.set_done();
    });

    std::thread consumer([&]{
        double sum = 0.0;
        double v;
        while (buffer.take(v)) {
            sum += v;
        }
        result = sum;
    });

    producer.join();
    consumer.join();

    auto end = std::chrono::steady_clock::now();
    double ms = std::chrono::duration<double, std::milli>(end - start).count();

    std::cout << "n=" << n << ", queueSize=" << queueSize << "\n";
    std::cout << "scalarProduct=" << result << "\n";
    std::cout << "timeMs=" << ms << "\n";

    // Correctness check
    double expected = 0.0;
    for (size_t i = 0; i < n; ++i) expected += a[i] * b[i];
    std::cout << "expected=" << expected << ", diff=" << std::abs(expected - result) << "\n";

    return 0;
}
