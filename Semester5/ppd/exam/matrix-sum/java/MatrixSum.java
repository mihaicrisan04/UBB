import java.util.Random;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.BrokenBarrierException;

public class MatrixSum {

    private static int[][] matrix;
    private static long[] partialSums;
    private static int numThreads;
    private static CyclicBarrier barrier;

    public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java MatrixSum <rows> <cols> <numThreads> <printMatrix(0/1)>");
            System.out.println("Note: numThreads should be a power of 2 for the binary tree");
            System.out.println("Example: java MatrixSum 100 100 4 0");
            return;
        }

        int rows = Integer.parseInt(args[0]);
        int cols = Integer.parseInt(args[1]);
        numThreads = Integer.parseInt(args[2]);
        boolean printMatrix = Integer.parseInt(args[3]) != 0;

        // Validate numThreads is power of 2
        if ((numThreads & (numThreads - 1)) != 0) {
            System.out.println("Warning: numThreads should be a power of 2 for binary tree. Using closest power of 2.");
            numThreads = Integer.highestOneBit(numThreads);
        }

        System.out.println("Matrix size: " + rows + "x" + cols);
        System.out.println("Threads:     " + numThreads);
        System.out.println();

        // Generate random matrix
        matrix = generateMatrix(rows, cols);

        if (printMatrix && rows <= 10 && cols <= 10) {
            printMatrix(matrix, "Matrix");
        }

        // Compute sum using binary tree reduction
        long startTime = System.nanoTime();
        long result = parallelSumBinaryTree(rows, cols);
        long endTime = System.nanoTime();

        long durationMicros = (endTime - startTime) / 1000;
        System.out.println("Sum (parallel): " + result);
        System.out.println("Computation time: " + durationMicros + " microseconds");

        // Verify with sequential sum
        long seqSum = sequentialSum();
        System.out.println("Sum (sequential verification): " + seqSum);
        System.out.println("Results match: " + (result == seqSum));
    }

    static int[][] generateMatrix(int rows, int cols) {
        Random rand = new Random();
        int[][] m = new int[rows][cols];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                m[i][j] = rand.nextInt(10) + 1;
            }
        }
        return m;
    }

    static long parallelSumBinaryTree(int rows, int cols) {
        int totalElements = rows * cols;
        partialSums = new long[numThreads];

        // Calculate number of levels in binary tree
        int levels = (int) (Math.log(numThreads) / Math.log(2));

        Thread[] threads = new Thread[numThreads];

        // Distribute elements among threads
        int elementsPerThread = totalElements / numThreads;
        int remainingElements = totalElements % numThreads;

        // Create barrier for synchronization at each tree level
        barrier = new CyclicBarrier(numThreads);

        int currentElement = 0;
        for (int t = 0; t < numThreads; t++) {
            int startIdx = currentElement;
            int extra = (t < remainingElements) ? 1 : 0;
            int endIdx = startIdx + elementsPerThread + extra;
            currentElement = endIdx;

            final int threadId = t;
            final int start = startIdx;
            final int end = endIdx;
            final int numLevels = levels;

            threads[t] = new Thread(() -> {
                // Phase 1: Compute partial sum for assigned elements
                long sum = 0;
                for (int idx = start; idx < end; idx++) {
                    int row = idx / matrix[0].length;
                    int col = idx % matrix[0].length;
                    sum += matrix[row][col];
                }
                partialSums[threadId] = sum;

                // Phase 2: Binary tree reduction
                for (int level = 0; level < numLevels; level++) {
                    try {
                        barrier.await(); // Synchronize before each level
                    } catch (InterruptedException | BrokenBarrierException e) {
                        Thread.currentThread().interrupt();
                        return;
                    }

                    // At each level, thread i combines with thread i + stride
                    int stride = 1 << level; // 2^level
                    int step = stride << 1;  // 2^(level+1)

                    if (threadId % step == 0) {
                        int partner = threadId + stride;
                        if (partner < numThreads) {
                            partialSums[threadId] += partialSums[partner];
                        }
                    }
                }
            });
            threads[t].start();
        }

        // Wait for all threads
        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        return partialSums[0];
    }

    static long sequentialSum() {
        long sum = 0;
        for (int[] row : matrix) {
            for (int val : row) {
                sum += val;
            }
        }
        return sum;
    }

    static void printMatrix(int[][] m, String name) {
        System.out.println(name + ":");
        for (int[] row : m) {
            for (int val : row) {
                System.out.print(val + "\t");
            }
            System.out.println();
        }
        System.out.println();
    }
}
