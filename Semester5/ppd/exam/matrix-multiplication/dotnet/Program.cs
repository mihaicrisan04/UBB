using System.Diagnostics;

class MatrixMultiply
{
    static void Main(string[] args)
    {
        if (args.Length < 5)
        {
            Console.WriteLine("Usage: dotnet run <rowsA> <colsA> <colsB> <numThreads> <printMatrices(0/1)>");
            Console.WriteLine("Example: dotnet run 4 3 5 2 1");
            return;
        }

        int rowsA = int.Parse(args[0]);
        int colsA = int.Parse(args[1]); // = rowsB
        int colsB = int.Parse(args[2]);
        int numThreads = int.Parse(args[3]);
        bool printResults = int.Parse(args[4]) != 0;

        Console.WriteLine($"Matrix A: {rowsA}x{colsA}");
        Console.WriteLine($"Matrix B: {colsA}x{colsB}");
        Console.WriteLine($"Result C: {rowsA}x{colsB}");
        Console.WriteLine($"Threads:  {numThreads}");
        Console.WriteLine();

        // Generate random matrices
        int[,] A = GenerateMatrix(rowsA, colsA);
        int[,] B = GenerateMatrix(colsA, colsB);
        int[,] C = new int[rowsA, colsB];

        if (printResults)
        {
            PrintMatrix(A, "Matrix A");
            PrintMatrix(B, "Matrix B");
        }

        // Perform parallel multiplication
        var stopwatch = Stopwatch.StartNew();
        ParallelMultiply(A, B, C, numThreads);
        stopwatch.Stop();

        if (printResults)
        {
            PrintMatrix(C, "Result C = A x B");
        }

        Console.WriteLine($"Computation time: {stopwatch.Elapsed.TotalMicroseconds:F0} microseconds");
    }

    static int[,] GenerateMatrix(int rows, int cols)
    {
        var rand = new Random();
        var matrix = new int[rows, cols];
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                matrix[i, j] = rand.Next(1, 11);
            }
        }
        return matrix;
    }

    static void ParallelMultiply(int[,] A, int[,] B, int[,] C, int numThreads)
    {
        int rowsA = A.GetLength(0);
        var threads = new Thread[numThreads];

        // Distribute rows among threads
        int rowsPerThread = rowsA / numThreads;
        int remainingRows = rowsA % numThreads;

        int currentRow = 0;
        for (int t = 0; t < numThreads; t++)
        {
            int startRow = currentRow;
            int extraRow = (t < remainingRows) ? 1 : 0;
            int endRow = startRow + rowsPerThread + extraRow;
            currentRow = endRow;

            if (startRow < endRow)
            {
                int start = startRow;
                int end = endRow;
                threads[t] = new Thread(() => MultiplyRows(A, B, C, start, end));
                threads[t].Start();
            }
        }

        // Wait for all threads
        foreach (var thread in threads)
        {
            thread?.Join();
        }
    }

    static void MultiplyRows(int[,] A, int[,] B, int[,] C, int startRow, int endRow)
    {
        int colsB = B.GetLength(1);
        int colsA = A.GetLength(1);

        for (int i = startRow; i < endRow; i++)
        {
            for (int j = 0; j < colsB; j++)
            {
                int sum = 0;
                for (int k = 0; k < colsA; k++)
                {
                    sum += A[i, k] * B[k, j];
                }
                C[i, j] = sum;
            }
        }
    }

    static void PrintMatrix(int[,] matrix, string name)
    {
        Console.WriteLine($"{name}:");
        int rows = matrix.GetLength(0);
        int cols = matrix.GetLength(1);
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                Console.Write($"{matrix[i, j]}\t");
            }
            Console.WriteLine();
        }
        Console.WriteLine();
    }
}
