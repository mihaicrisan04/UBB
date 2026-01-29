using System.Diagnostics;

class ScalarProduct
{
    static void Main(string[] args)
    {
        if (args.Length < 3)
        {
            Console.WriteLine("Usage: dotnet run <vectorSize> <numThreads> <printVectors(0/1)>");
            Console.WriteLine("Example: dotnet run 1000000 4 0");
            return;
        }

        int size = int.Parse(args[0]);
        int numThreads = int.Parse(args[1]);
        bool printVectors = int.Parse(args[2]) != 0;

        Console.WriteLine($"Vector size: {size}");
        Console.WriteLine($"Threads:     {numThreads}");
        Console.WriteLine();

        // Generate random vectors
        int[] A = GenerateVector(size);
        int[] B = GenerateVector(size);

        if (printVectors && size <= 20)
        {
            PrintVector(A, "Vector A");
            PrintVector(B, "Vector B");
        }

        // Compute scalar product in parallel
        var stopwatch = Stopwatch.StartNew();
        long result = ParallelScalarProduct(A, B, numThreads);
        stopwatch.Stop();

        Console.WriteLine($"Scalar product: {result}");
        Console.WriteLine($"Computation time: {stopwatch.Elapsed.TotalMicroseconds:F0} microseconds");
    }

    static int[] GenerateVector(int size)
    {
        var rand = new Random();
        var vector = new int[size];
        for (int i = 0; i < size; i++)
        {
            vector[i] = rand.Next(1, 11);
        }
        return vector;
    }

    static long ParallelScalarProduct(int[] A, int[] B, int numThreads)
    {
        int size = A.Length;
        var threads = new Thread[numThreads];
        var partialSums = new long[numThreads];

        // Distribute elements among threads
        int elementsPerThread = size / numThreads;
        int remainingElements = size % numThreads;

        int currentIndex = 0;
        for (int t = 0; t < numThreads; t++)
        {
            int startIdx = currentIndex;
            int extra = (t < remainingElements) ? 1 : 0;
            int endIdx = startIdx + elementsPerThread + extra;
            currentIndex = endIdx;

            int threadId = t;
            int start = startIdx;
            int end = endIdx;
            threads[t] = new Thread(() =>
            {
                long sum = 0;
                for (int i = start; i < end; i++)
                {
                    sum += (long)A[i] * B[i];
                }
                partialSums[threadId] = sum;
            });
            threads[t].Start();
        }

        // Wait for all threads
        foreach (var thread in threads)
        {
            thread.Join();
        }

        // Combine partial sums
        long totalSum = 0;
        foreach (var sum in partialSums)
        {
            totalSum += sum;
        }
        return totalSum;
    }

    static void PrintVector(int[] vector, string name)
    {
        Console.WriteLine($"{name}: [{string.Join(", ", vector)}]");
    }
}
