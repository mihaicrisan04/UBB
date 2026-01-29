import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class MatrixMultiply {

    public static void main(String[] args) {
        if (args.length < 5) {
            System.out.println("Usage: java MatrixMultiply <rowsA> <colsA> <colsB> <numThreads> <printMatrices(0/1)>");
            System.out.println("Example: java MatrixMultiply 4 3 5 2 1");
            return;
        }

        int rowsA = Integer.parseInt(args[0]);
        int colsA = Integer.parseInt(args[1]); // = rowsB
        int colsB = Integer.parseInt(args[2]);
        int numThreads = Integer.parseInt(args[3]);
        boolean printResults = Integer.parseInt(args[4]) != 0;

        System.out.println("Matrix A: " + rowsA + "x" + colsA);
        System.out.println("Matrix B: " + colsA + "x" + colsB);
        System.out.println("Result C: " + rowsA + "x" + colsB);
        System.out.println("Threads:  " + numThreads);
        System.out.println();

        // Generate random matrices
        int[][] A = generateMatrix(rowsA, colsA);
        int[][] B = generateMatrix(colsA, colsB);
        int[][] C = new int[rowsA][colsB];

        if (printResults) {
            printMatrix(A, "Matrix A");
            printMatrix(B, "Matrix B");
        }

        // Perform parallel multiplication
        long startTime = System.nanoTime();
        parallelMultiply(A, B, C, numThreads);
        long endTime = System.nanoTime();

        if (printResults) {
            printMatrix(C, "Result C = A x B");
        }

        long durationMicros = (endTime - startTime) / 1000;
        System.out.println("Computation time: " + durationMicros + " microseconds");
    }

    static int[][] generateMatrix(int rows, int cols) {
        Random rand = new Random();
        int[][] matrix = new int[rows][cols];
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                matrix[i][j] = rand.nextInt(10) + 1;
            }
        }
        return matrix;
    }

    static void parallelMultiply(int[][] A, int[][] B, int[][] C, int numThreads) {
        int rowsA = A.length;
        ExecutorService executor = Executors.newFixedThreadPool(numThreads);

        // Distribute rows among threads
        int rowsPerThread = rowsA / numThreads;
        int remainingRows = rowsA % numThreads;

        int currentRow = 0;
        for (int t = 0; t < numThreads; t++) {
            int startRow = currentRow;
            int extraRow = (t < remainingRows) ? 1 : 0;
            int endRow = startRow + rowsPerThread + extraRow;
            currentRow = endRow;

            if (startRow < endRow) {
                final int start = startRow;
                final int end = endRow;
                executor.submit(() -> multiplyRows(A, B, C, start, end));
            }
        }

        executor.shutdown();
        try {
            executor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    static void multiplyRows(int[][] A, int[][] B, int[][] C, int startRow, int endRow) {
        int colsB = B[0].length;
        int colsA = A[0].length;

        for (int i = startRow; i < endRow; i++) {
            for (int j = 0; j < colsB; j++) {
                int sum = 0;
                for (int k = 0; k < colsA; k++) {
                    sum += A[i][k] * B[k][j];
                }
                C[i][j] = sum;
            }
        }
    }

    static void printMatrix(int[][] matrix, String name) {
        System.out.println(name + ":");
        for (int[] row : matrix) {
            for (int val : row) {
                System.out.print(val + "\t");
            }
            System.out.println();
        }
        System.out.println();
    }
}
