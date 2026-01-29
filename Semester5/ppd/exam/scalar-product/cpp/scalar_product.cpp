#include <iostream>
#include <vector>
#include <thread>
#include <chrono>
#include <random>

// Worker function: computes partial sum for elements [start, end)
void computePartialSum(const std::vector<int>& A, const std::vector<int>& B,
                       long long& partialSum, int start, int end) {
    long long sum = 0;
    for (int i = start; i < end; i++) {
        sum += static_cast<long long>(A[i]) * B[i];
    }
    partialSum = sum;
}

long long parallelScalarProduct(const std::vector<int>& A, const std::vector<int>& B,
                                 int numThreads) {
    int size = A.size();
    std::vector<std::thread> threads;
    std::vector<long long> partialSums(numThreads, 0);

    // Distribute elements among threads
    int elementsPerThread = size / numThreads;
    int remainingElements = size % numThreads;

    int currentIndex = 0;
    for (int t = 0; t < numThreads; t++) {
        int startIdx = currentIndex;
        int extra = (t < remainingElements) ? 1 : 0;
        int endIdx = startIdx + elementsPerThread + extra;
        currentIndex = endIdx;

        if (startIdx < endIdx) {
            threads.emplace_back(computePartialSum, std::cref(A), std::cref(B),
                                 std::ref(partialSums[t]), startIdx, endIdx);
        }
    }

    // Wait for all threads
    for (auto& thread : threads) {
        thread.join();
    }

    // Combine partial sums
    long long totalSum = 0;
    for (const auto& sum : partialSums) {
        totalSum += sum;
    }
    return totalSum;
}

std::vector<int> generateVector(int size) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 10);

    std::vector<int> vec(size);
    for (int i = 0; i < size; i++) {
        vec[i] = dis(gen);
    }
    return vec;
}

void printVector(const std::vector<int>& vec, const std::string& name) {
    std::cout << name << ": [";
    for (size_t i = 0; i < vec.size(); i++) {
        std::cout << vec[i];
        if (i < vec.size() - 1) std::cout << ", ";
    }
    std::cout << "]\n";
}

int main(int argc, char* argv[]) {
    if (argc < 4) {
        std::cerr << "Usage: " << argv[0] << " <vectorSize> <numThreads> <printVectors(0/1)>\n";
        std::cerr << "Example: " << argv[0] << " 1000000 4 0\n";
        return 1;
    }

    int size = std::stoi(argv[1]);
    int numThreads = std::stoi(argv[2]);
    bool printVectors = std::stoi(argv[3]) != 0;

    std::cout << "Vector size: " << size << "\n";
    std::cout << "Threads:     " << numThreads << "\n\n";

    // Generate random vectors
    std::vector<int> A = generateVector(size);
    std::vector<int> B = generateVector(size);

    if (printVectors && size <= 20) {
        printVector(A, "Vector A");
        printVector(B, "Vector B");
    }

    // Compute scalar product in parallel
    auto start = std::chrono::high_resolution_clock::now();
    long long result = parallelScalarProduct(A, B, numThreads);
    auto end = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);

    std::cout << "Scalar product: " << result << "\n";
    std::cout << "Computation time: " << duration.count() << " microseconds\n";

    return 0;
}
