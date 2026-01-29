import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class ScalarProduct {

    public static void main(String[] args) {
        if (args.length < 3) {
            System.out.println("Usage: java ScalarProduct <vectorSize> <numThreads> <printVectors(0/1)>");
            System.out.println("Example: java ScalarProduct 1000000 4 0");
            return;
        }

        int size = Integer.parseInt(args[0]);
        int numThreads = Integer.parseInt(args[1]);
        boolean printVectors = Integer.parseInt(args[2]) != 0;

        System.out.println("Vector size: " + size);
        System.out.println("Threads:     " + numThreads);
        System.out.println();

        // Generate random vectors
        int[] A = generateVector(size);
        int[] B = generateVector(size);

        if (printVectors && size <= 20) {
            printVector(A, "Vector A");
            printVector(B, "Vector B");
        }

        // Compute scalar product in parallel
        long startTime = System.nanoTime();
        long result = parallelScalarProduct(A, B, numThreads);
        long endTime = System.nanoTime();

        long durationMicros = (endTime - startTime) / 1000;
        System.out.println("Scalar product: " + result);
        System.out.println("Computation time: " + durationMicros + " microseconds");
    }

    static int[] generateVector(int size) {
        Random rand = new Random();
        int[] vector = new int[size];
        for (int i = 0; i < size; i++) {
            vector[i] = rand.nextInt(10) + 1;
        }
        return vector;
    }

    static long parallelScalarProduct(int[] A, int[] B, int numThreads) {
        int size = A.length;
        long[] partialSums = new long[numThreads];
        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        // Distribute elements among threads
        int elementsPerThread = size / numThreads;
        int remainingElements = size % numThreads;

        int currentIndex = 0;
        for (int t = 0; t < numThreads; t++) {
            int startIdx = currentIndex;
            int extra = (t < remainingElements) ? 1 : 0;
            int endIdx = startIdx + elementsPerThread + extra;
            currentIndex = endIdx;

            final int threadId = t;
            final int start = startIdx;
            final int end = endIdx;
            executor.submit(() -> {
                long sum = 0;
                for (int i = start; i < end; i++) {
                    sum += (long) A[i] * B[i];
                }
                partialSums[threadId] = sum;
            });
        }

        executor.shutdown();
        try {
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        // Combine partial sums
        long totalSum = 0;
        for (long sum : partialSums) {
            totalSum += sum;
        }
        return totalSum;
    }

    static void printVector(int[] vector, String name) {
        StringBuilder sb = new StringBuilder();
        sb.append(name).append(": [");
        for (int i = 0; i < vector.length; i++) {
            sb.append(vector[i]);
            if (i < vector.length - 1) sb.append(", ");
        }
        sb.append("]");
        System.out.println(sb.toString());
    }
}
