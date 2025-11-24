package ppd.lab05;

import java.util.*;
import java.util.concurrent.*;

/**
 * Polynomial multiplication algorithms.
 * Implements both naive O(n^2) and Karatsuba O(n^1.585) in sequential and parallel variants.
 */
public class PolynomialMultiply {

    // ========================================================================
    // Naive O(n^2) Algorithm
    // ========================================================================

    /**
     * Naive sequential polynomial multiplication.
     * Complexity: O(n*m) where n = |A|, m = |B|
     *
     * For each output coefficient C[k], compute sum of A[i]*B[k-i] for valid i.
     */
    public static Polynomial multiplyNaiveSeq(Polynomial A, Polynomial B) {
        int n = A.size();
        int m = B.size();

        if (n == 0 || m == 0) {
            return new Polynomial(new long[]{0});
        }

        long[] result = new long[n + m - 1];

        for (int i = 0; i < n; i++) {
            long ai = A.getCoeff(i);
            for (int j = 0; j < m; j++) {
                result[i + j] += ai * B.getCoeff(j);
            }
        }

        return new Polynomial(result);
    }

    /**
     * Naive parallel polynomial multiplication.
     *
     * Synchronization strategy:
     * - Partition the output index space [0, n+m-2] across threads
     * - Each thread computes a disjoint range of result coefficients
     * - No locks needed since each thread writes to distinct indices
     * - Implicit barrier via waiting for all futures to complete
     *
     * @param numThreads number of worker threads
     */
    public static Polynomial multiplyNaivePar(Polynomial A, Polynomial B, int numThreads) {
        int n = A.size();
        int m = B.size();

        if (n == 0 || m == 0) {
            return new Polynomial(new long[]{0});
        }

        int resultSize = n + m - 1;

        // For small problems, sequential is faster
        if (numThreads <= 1 || resultSize < 2048) {
            return multiplyNaiveSeq(A, B);
        }

        long[] result = new long[resultSize];
        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        try {
            List<Future<?>> futures = new ArrayList<>();
            int chunkSize = (resultSize + numThreads - 1) / numThreads;

            for (int t = 0; t < numThreads; t++) {
                final int start = t * chunkSize;
                final int end = Math.min(resultSize, start + chunkSize);

                if (start >= resultSize) break;

                futures.add(executor.submit(() -> {
                    // Compute C[k] for k in [start, end)
                    for (int k = start; k < end; k++) {
                        // C[k] = sum_{i} A[i] * B[k-i]
                        // Valid i: 0 <= i < n and 0 <= k-i < m
                        int iMin = Math.max(0, k - m + 1);
                        int iMax = Math.min(k, n - 1);

                        long sum = 0;
                        for (int i = iMin; i <= iMax; i++) {
                            sum += A.getCoeff(i) * B.getCoeff(k - i);
                        }
                        result[k] = sum;
                    }
                }));
            }

            // Wait for all tasks to complete (barrier)
            for (Future<?> future : futures) {
                future.get();
            }

        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException("Parallel computation failed", e);
        } finally {
            executor.shutdown();
        }

        return new Polynomial(result);
    }

    // ========================================================================
    // Karatsuba Algorithm
    // ========================================================================

    /**
     * Karatsuba sequential polynomial multiplication.
     * Complexity: O(n^log2(3)) ≈ O(n^1.585)
     *
     * Algorithm:
     * 1. Split A and B into halves: A = A0 + A1*x^m, B = B0 + B1*x^m
     * 2. Compute three products:
     *    - z0 = A0 * B0
     *    - z2 = A1 * B1
     *    - z1 = (A0 + A1) * (B0 + B1) - z0 - z2
     * 3. Combine: C = z0 + z1*x^m + z2*x^(2m)
     *
     * @param seqBase threshold below which to use naive multiplication
     */
    public static Polynomial multiplyKaratsubaSeq(Polynomial A, Polynomial B, int seqBase) {
        // Pad to same size (power of 2)
        int maxSize = Math.max(A.size(), B.size());
        int paddedSize = 1;
        while (paddedSize < maxSize) {
            paddedSize <<= 1;
        }

        long[] aPadded = A.getCoeffsPadded(paddedSize);
        long[] bPadded = B.getCoeffsPadded(paddedSize);

        long[] result = karatsubaRecSeq(aPadded, bPadded, seqBase);

        // Trim to correct size
        int correctSize = A.size() + B.size() - 1;
        return new Polynomial(Arrays.copyOf(result, correctSize));
    }

    private static long[] karatsubaRecSeq(long[] a, long[] b, int seqBase) {
        int n = a.length;

        // Base case: use naive multiplication
        if (n <= seqBase) {
            return multiplyNaiveSmall(a, b);
        }

        int mid = n / 2;

        // Split into halves
        long[] a0 = Arrays.copyOfRange(a, 0, mid);
        long[] a1 = Arrays.copyOfRange(a, mid, n);
        long[] b0 = Arrays.copyOfRange(b, 0, mid);
        long[] b1 = Arrays.copyOfRange(b, mid, n);

        // Three recursive multiplications
        long[] z0 = karatsubaRecSeq(a0, b0, seqBase);
        long[] z2 = karatsubaRecSeq(a1, b1, seqBase);

        long[] a01 = addArrays(a0, a1);
        long[] b01 = addArrays(b0, b1);
        long[] z1Full = karatsubaRecSeq(a01, b01, seqBase);
        long[] z1 = subtractArrays(subtractArrays(z1Full, z0), z2);

        // Combine: result = z0 + z1*x^mid + z2*x^(2*mid)
        long[] result = new long[2 * n - 1];

        // Add z0
        for (int i = 0; i < z0.length; i++) {
            result[i] += z0[i];
        }

        // Add z1 shifted by mid
        for (int i = 0; i < z1.length; i++) {
            result[i + mid] += z1[i];
        }

        // Add z2 shifted by 2*mid
        for (int i = 0; i < z2.length; i++) {
            result[i + 2 * mid] += z2[i];
        }

        return result;
    }

    /**
     * Karatsuba parallel polynomial multiplication.
     *
     * Synchronization strategy:
     * - Uses ExecutorService with a fixed thread pool
     * - Independent subproblems (z0, z2) are submitted as parallel tasks
     * - Parent thread computes z1 or waits for submitted tasks
     * - Recombination happens in parent thread (no concurrent writes)
     * - Parallelization controlled by:
     *   - maxDepth: limits recursion depth for parallelism
     *   - parBase: minimum problem size to parallelize
     *
     * @param numThreads number of worker threads
     * @param seqBase threshold for switching to naive
     * @param parBase threshold for enabling parallelism
     * @param maxDepth maximum parallel recursion depth
     */
    public static Polynomial multiplyKaratsubaPar(Polynomial A, Polynomial B, int numThreads,
                                                   int seqBase, int parBase, int maxDepth) {
        if (numThreads <= 1) {
            return multiplyKaratsubaSeq(A, B, seqBase);
        }

        // Pad to same size (power of 2)
        int maxSize = Math.max(A.size(), B.size());
        int paddedSize = 1;
        while (paddedSize < maxSize) {
            paddedSize <<= 1;
        }

        long[] aPadded = A.getCoeffsPadded(paddedSize);
        long[] bPadded = B.getCoeffsPadded(paddedSize);

        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        try {
            long[] result = karatsubaRecPar(aPadded, bPadded, seqBase, parBase, maxDepth, 0, executor);

            // Trim to correct size
            int correctSize = A.size() + B.size() - 1;
            return new Polynomial(Arrays.copyOf(result, correctSize));
        } finally {
            executor.shutdown();
        }
    }

    private static long[] karatsubaRecPar(long[] a, long[] b, int seqBase, int parBase,
                                          int maxDepth, int depth, ExecutorService executor) {
        int n = a.length;

        // Base case: use naive multiplication
        if (n <= seqBase) {
            return multiplyNaiveSmall(a, b);
        }

        int mid = n / 2;

        // Split into halves
        long[] a0 = Arrays.copyOfRange(a, 0, mid);
        long[] a1 = Arrays.copyOfRange(a, mid, n);
        long[] b0 = Arrays.copyOfRange(b, 0, mid);
        long[] b1 = Arrays.copyOfRange(b, mid, n);

        boolean doParallel = depth < maxDepth && n >= parBase;

        long[] z0, z1, z2;

        if (doParallel) {
            // Parallelize z0 and z2
            Future<long[]> futureZ0 = executor.submit(() ->
                karatsubaRecPar(a0, b0, seqBase, parBase, maxDepth, depth + 1, executor)
            );
            Future<long[]> futureZ2 = executor.submit(() ->
                karatsubaRecPar(a1, b1, seqBase, parBase, maxDepth, depth + 1, executor)
            );

            // Compute z1 in current thread
            long[] a01 = addArrays(a0, a1);
            long[] b01 = addArrays(b0, b1);
            long[] z1Full = karatsubaRecPar(a01, b01, seqBase, parBase, maxDepth, depth + 1, executor);

            try {
                z0 = futureZ0.get();
                z2 = futureZ2.get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException("Parallel Karatsuba failed", e);
            }

            z1 = subtractArrays(subtractArrays(z1Full, z0), z2);
        } else {
            // Sequential execution
            z0 = karatsubaRecPar(a0, b0, seqBase, parBase, maxDepth, depth + 1, executor);
            z2 = karatsubaRecPar(a1, b1, seqBase, parBase, maxDepth, depth + 1, executor);

            long[] a01 = addArrays(a0, a1);
            long[] b01 = addArrays(b0, b1);
            long[] z1Full = karatsubaRecPar(a01, b01, seqBase, parBase, maxDepth, depth + 1, executor);
            z1 = subtractArrays(subtractArrays(z1Full, z0), z2);
        }

        // Combine: result = z0 + z1*x^mid + z2*x^(2*mid)
        long[] result = new long[2 * n - 1];

        for (int i = 0; i < z0.length; i++) {
            result[i] += z0[i];
        }
        for (int i = 0; i < z1.length; i++) {
            result[i + mid] += z1[i];
        }
        for (int i = 0; i < z2.length; i++) {
            result[i + 2 * mid] += z2[i];
        }

        return result;
    }

    // ========================================================================
    // Helper methods
    // ========================================================================

    /**
     * Simple naive multiplication for small arrays.
     */
    private static long[] multiplyNaiveSmall(long[] a, long[] b) {
        int n = a.length;
        int m = b.length;
        long[] result = new long[n + m - 1];

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                result[i + j] += a[i] * b[j];
            }
        }

        return result;
    }

    /**
     * Adds two arrays element-wise.
     */
    private static long[] addArrays(long[] a, long[] b) {
        int maxLen = Math.max(a.length, b.length);
        long[] result = new long[maxLen];

        for (int i = 0; i < maxLen; i++) {
            long ai = i < a.length ? a[i] : 0;
            long bi = i < b.length ? b[i] : 0;
            result[i] = ai + bi;
        }

        return result;
    }

    /**
     * Subtracts array b from array a element-wise.
     */
    private static long[] subtractArrays(long[] a, long[] b) {
        int maxLen = Math.max(a.length, b.length);
        long[] result = new long[maxLen];

        for (int i = 0; i < maxLen; i++) {
            long ai = i < a.length ? a[i] : 0;
            long bi = i < b.length ? b[i] : 0;
            result[i] = ai - bi;
        }

        return result;
    }
}
