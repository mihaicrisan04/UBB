import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class MatrixMultiply {
    private int[][] A, B, C;
    private int N, M, P; // A is NxM, B is MxP, C is NxP
    private int numThreads;
    private boolean debug;

    public MatrixMultiply(int n, int m, int p, int numThreads, boolean debug) {
        this.N = n;
        this.M = m;
        this.P = p;
        this.numThreads = numThreads;
        this.debug = debug;

        A = new int[N][M];
        B = new int[M][P];
        C = new int[N][P];

        initializeMatrices();
    }

    private void initializeMatrices() {
        Random rand = new Random(42); // Fixed seed for reproducibility

        for (int i = 0; i < N; i++) {
            for (int j = 0; j < M; j++) {
                A[i][j] = rand.nextInt(10);
            }
        }

        for (int i = 0; i < M; i++) {
            for (int j = 0; j < P; j++) {
                B[i][j] = rand.nextInt(10);
            }
        }
    }

    // Compute a single element of the result matrix
    private void computeElement(int row, int col, int threadId) {
        if (debug) {
            System.out.printf("Thread %d computing element (%d, %d)%n", threadId, row, col);
        }

        int sum = 0;
        for (int k = 0; k < M; k++) {
            sum += A[row][k] * B[k][col];
        }
        C[row][col] = sum;
    }

    // Strategy 1: Row-by-row consecutive elements
    class RowByRowTask implements Runnable {
        private int threadId;
        private CountDownLatch latch;

        public RowByRowTask(int threadId, CountDownLatch latch) {
            this.threadId = threadId;
            this.latch = latch;
        }

        @Override
        public void run() {
            int totalElements = N * P;
            int elementsPerThread = totalElements / numThreads;
            int start = threadId * elementsPerThread;
            int end = (threadId == numThreads - 1) ? totalElements : start + elementsPerThread;

            for (int idx = start; idx < end; idx++) {
                int row = idx / P;
                int col = idx % P;
                computeElement(row, col, threadId);
            }

            latch.countDown();
        }
    }

    // Strategy 2: Column-by-column consecutive elements
    class ColByColTask implements Runnable {
        private int threadId;
        private CountDownLatch latch;

        public ColByColTask(int threadId, CountDownLatch latch) {
            this.threadId = threadId;
            this.latch = latch;
        }

        @Override
        public void run() {
            int totalElements = N * P;
            int elementsPerThread = totalElements / numThreads;
            int start = threadId * elementsPerThread;
            int end = (threadId == numThreads - 1) ? totalElements : start + elementsPerThread;

            for (int idx = start; idx < end; idx++) {
                int col = idx / N;
                int row = idx % N;
                computeElement(row, col, threadId);
            }

            latch.countDown();
        }
    }

    // Strategy 3: Every k-th element
    class KthElementTask implements Runnable {
        private int threadId;
        private CountDownLatch latch;

        public KthElementTask(int threadId, CountDownLatch latch) {
            this.threadId = threadId;
            this.latch = latch;
        }

        @Override
        public void run() {
            int totalElements = N * P;

            for (int idx = threadId; idx < totalElements; idx += numThreads) {
                int row = idx / P;
                int col = idx % P;
                computeElement(row, col, threadId);
            }

            latch.countDown();
        }
    }

    // Run multiplication with specified strategy
    public double runMultiplication(int strategy) throws InterruptedException {
        // Initialize result matrix
        C = new int[N][P];

        CountDownLatch latch = new CountDownLatch(numThreads);
        Thread[] threads = new Thread[numThreads];

        long startTime = System.nanoTime();

        // Create and start threads
        for (int i = 0; i < numThreads; i++) {
            Runnable task;
            switch (strategy) {
                case 0:
                    task = new RowByRowTask(i, latch);
                    break;
                case 1:
                    task = new ColByColTask(i, latch);
                    break;
                case 2:
                    task = new KthElementTask(i, latch);
                    break;
                default:
                    task = new RowByRowTask(i, latch);
            }

            threads[i] = new Thread(task);
            threads[i].start();
        }

        // Wait for all threads to complete
        latch.await();

        long endTime = System.nanoTime();
        return (endTime - startTime) / 1_000_000.0; // Convert to milliseconds
    }

    // Verify result (optional, for small matrices)
    public void verifyResult() {
        int[][] expected = new int[N][P];

        // Compute sequentially
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < P; j++) {
                int sum = 0;
                for (int k = 0; k < M; k++) {
                    sum += A[i][k] * B[k][j];
                }
                expected[i][j] = sum;
            }
        }

        // Compare
        int errors = 0;
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < P; j++) {
                if (C[i][j] != expected[i][j]) {
                    errors++;
                    if (errors <= 5) {
                        System.out.printf("Error at (%d, %d): expected %d, got %d%n",
                                i, j, expected[i][j], C[i][j]);
                    }
                }
            }
        }

        if (errors == 0) {
            System.out.println("✓ Result verified successfully");
        } else {
            System.out.printf("✗ Found %d errors%n", errors);
        }
    }

    public static void main(String[] args) {
        if (args.length < 4) {
            System.out.println("Usage: java MatrixMultiply <rows_A> <cols_A> <cols_B> <num_threads> [debug]");
            System.out.println("  Example: java MatrixMultiply 1000 1000 1000 4");
            System.out.println("  Add 'debug' at the end to enable debug output (only for small matrices!)");
            return;
        }

        int n = Integer.parseInt(args[0]);
        int m = Integer.parseInt(args[1]);
        int p = Integer.parseInt(args[2]);
        int numThreads = Integer.parseInt(args[3]);
        boolean debug = args.length > 4 && args[4].equals("debug");

        System.out.printf("Matrix multiplication: A(%dx%d) * B(%dx%d) = C(%dx%d)%n", n, m, m, p, n, p);
        System.out.printf("Number of threads: %d%n%n", numThreads);

        MatrixMultiply mm = new MatrixMultiply(n, m, p, numThreads, debug);

        String[] strategyNames = {
            "Row-by-row consecutive elements",
            "Column-by-column consecutive elements",
            "Every k-th element"
        };

        try {
            // Run all three strategies
            for (int strategy = 0; strategy < 3; strategy++) {
                System.out.printf("Strategy %d: %s%n", strategy + 1, strategyNames[strategy]);
                double time = mm.runMultiplication(strategy);
                System.out.printf("  Time: %.2f ms%n", time);

                if (n <= 10 && p <= 10 && debug) {
                    mm.verifyResult();
                }
                System.out.println();
            }
        } catch (InterruptedException e) {
            System.err.println("Thread execution interrupted: " + e.getMessage());
            e.printStackTrace();
        }
    }
}