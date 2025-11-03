public class Benchmark {

    public static void runExperiment(String title, int[][] configs) {
        System.out.println(title);
        System.out.println("-".repeat(title.length()));

        for (int[] config : configs) {
            int n = config[0];
            int m = config[1];
            int p = config[2];
            int threads = config[3];

            System.out.printf("%nMatrix: %dx%d * %dx%d, Threads: %d%n", n, m, m, p, threads);

            MatrixMultiply mm = new MatrixMultiply(n, m, p, threads, false);

            String[] strategyNames = {
                "Row-by-row",
                "Column-by-column",
                "Every k-th element"
            };

            try {
                for (int strategy = 0; strategy < 3; strategy++) {
                    double time = mm.runMultiplication(strategy);
                    System.out.printf("  Strategy %d (%s): %.2f ms%n",
                            strategy + 1, strategyNames[strategy], time);
                }
            } catch (InterruptedException e) {
                System.err.println("Error: " + e.getMessage());
            }
        }
        System.out.println();
    }

    public static void main(String[] args) {
        System.out.println("Matrix Multiplication Threading Performance Benchmark");
        System.out.println("======================================================");
        System.out.println();

        // Experiment 1: Varying matrix sizes (fixed 4 threads)
        System.out.println();
        int[][] exp1 = {
            {100, 100, 100, 4},
            {500, 500, 500, 4},
            {1000, 1000, 1000, 4},
            {1500, 1500, 1500, 4},
            {2000, 2000, 2000, 4}
        };
        runExperiment("Experiment 1: Varying matrix sizes (fixed 4 threads)", exp1);

        // Experiment 2: Varying number of threads (fixed 1000x1000 matrix)
        System.out.println();
        int[][] exp2 = {
            {1000, 1000, 1000, 1},
            {1000, 1000, 1000, 2},
            {1000, 1000, 1000, 4},
            {1000, 1000, 1000, 8},
            {1000, 1000, 1000, 16}
        };
        runExperiment("Experiment 2: Varying number of threads (fixed 1000x1000 matrix)", exp2);

        // Experiment 3: Cache effects - rectangular matrices
        System.out.println();
        int[][] exp3 = {
            {1000, 1000, 1000, 4},  // Square
            {2000, 500, 500, 4},     // Tall
            {500, 2000, 2000, 4}     // Wide
        };
        runExperiment("Experiment 3: Cache effects - rectangular matrices", exp3);

        // Experiment 4: Small matrix with debug
        System.out.println();
        System.out.println("Experiment 4: Debug output (9x9 matrix, 4 threads)");
        System.out.println("---------------------------------------------------");
        System.out.println();

        MatrixMultiply mm = new MatrixMultiply(9, 9, 9, 4, true);
        String[] strategyNames = {
            "Row-by-row consecutive elements",
            "Column-by-column consecutive elements",
            "Every k-th element"
        };

        try {
            for (int strategy = 0; strategy < 3; strategy++) {
                System.out.printf("Strategy %d: %s%n", strategy + 1, strategyNames[strategy]);
                double time = mm.runMultiplication(strategy);
                System.out.printf("Time: %.2f ms%n", time);
                mm.verifyResult();
                System.out.println();
            }
        } catch (InterruptedException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
