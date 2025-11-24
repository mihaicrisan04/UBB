package ppd.lab05;

import java.util.*;
import java.util.function.*;

/**
 * Polynomial Multiplication (lab05) - Main entry point
 *
 * Implements and benchmarks 4 variants:
 * - Naive O(n^2): sequential and parallel
 * - Karatsuba: sequential and parallel
 */
public class Main {

    public static void main(String[] args) {
        try {
            Config config = parseArgs(args);

            // Generate polynomials with all coefficients = 1
            Polynomial A = Polynomial.allOnes(config.size);
            Polynomial B = Polynomial.allOnes(config.size);

            // Print configuration
            System.out.println("Config:");
            System.out.println("  algorithm = " + config.algorithm);
            System.out.println("  mode      = " + config.mode);
            System.out.println("  threads   = " + config.threads);
            System.out.println("  size      = " + config.size);
            System.out.println("  repeats   = " + config.repeats);
            System.out.println("  compare   = " + config.compare);
            System.out.println("  verify    = " + config.verify);
            System.out.println("  karatsuba: seqBase=" + config.karatsubaSeqBase +
                             " parBase=" + config.karatsubaParBase +
                             " maxDepth=" + config.karatsubaMaxDepth);
            System.out.println();
            
            // Print polynomials
            System.out.println("Polynomial A (size=" + A.size() + "):");
            System.out.println("  " + java.util.Arrays.toString(A.getCoeffs()));
            System.out.println();
            System.out.println("Polynomial B (size=" + B.size() + "):");
            System.out.println("  " + java.util.Arrays.toString(B.getCoeffs()));
            System.out.println();

            if (config.compare) {
                runComparison(config, A, B);
            } else {
                runSingle(config, A, B);
            }

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    private static void runComparison(Config config, Polynomial A, Polynomial B) {
        List<BenchResult> results = new ArrayList<>();

        // Naive sequential
        results.add(benchmark("naive seq", config, A, B,
            () -> PolynomialMultiply.multiplyNaiveSeq(A, B)));

        // Naive parallel
        results.add(benchmark("naive par", config, A, B,
            () -> PolynomialMultiply.multiplyNaivePar(A, B, config.threads)));

        // Karatsuba sequential
        results.add(benchmark("karatsuba seq", config, A, B,
            () -> PolynomialMultiply.multiplyKaratsubaSeq(A, B, config.karatsubaSeqBase)));

        // Karatsuba parallel
        results.add(benchmark("karatsuba par", config, A, B,
            () -> PolynomialMultiply.multiplyKaratsubaPar(A, B, config.threads,
                config.karatsubaSeqBase, config.karatsubaParBase, config.karatsubaMaxDepth)));

        if (config.verify) {
            System.out.println("\nVerification:");
            boolean naiveOk = results.get(0).result.equals(results.get(1).result);
            boolean karaOk = results.get(2).result.equals(results.get(3).result);
            boolean crossOk = results.get(0).result.equals(results.get(2).result);

            System.out.println("  naive par == naive seq: " + (naiveOk ? "OK" : "MISMATCH"));
            System.out.println("  kara  par == kara  seq: " + (karaOk ? "OK" : "MISMATCH"));
            System.out.println("  kara  seq == naive seq: " + (crossOk ? "OK" : "MISMATCH"));
            System.out.println();
        }
        
        // Print result polynomial
        System.out.println("Result polynomial (size=" + results.get(0).result.size() + "):");
        System.out.println("  " + java.util.Arrays.toString(results.get(0).result.getCoeffs()));
        System.out.println();
    }

    private static void runSingle(Config config, Polynomial A, Polynomial B) {
        Supplier<Polynomial> fn;
        String label;

        if (config.algorithm.equalsIgnoreCase("naive") && config.mode.equalsIgnoreCase("seq")) {
            label = "naive seq";
            fn = () -> PolynomialMultiply.multiplyNaiveSeq(A, B);
        } else if (config.algorithm.equalsIgnoreCase("naive") && config.mode.equalsIgnoreCase("par")) {
            label = "naive par";
            fn = () -> PolynomialMultiply.multiplyNaivePar(A, B, config.threads);
        } else if (config.algorithm.equalsIgnoreCase("karatsuba") && config.mode.equalsIgnoreCase("seq")) {
            label = "karatsuba seq";
            fn = () -> PolynomialMultiply.multiplyKaratsubaSeq(A, B, config.karatsubaSeqBase);
        } else if (config.algorithm.equalsIgnoreCase("karatsuba") && config.mode.equalsIgnoreCase("par")) {
            label = "karatsuba par";
            fn = () -> PolynomialMultiply.multiplyKaratsubaPar(A, B, config.threads,
                config.karatsubaSeqBase, config.karatsubaParBase, config.karatsubaMaxDepth);
        } else {
            throw new IllegalArgumentException("Invalid algorithm/mode combination");
        }

        BenchResult result = benchmark(label, config, A, B, fn);
        
        // Print result polynomial
        System.out.println("\nResult polynomial (size=" + result.result.size() + "):");
        System.out.println("  " + java.util.Arrays.toString(result.result.getCoeffs()));
        System.out.println();

        if (config.verify) {
            Polynomial ref = PolynomialMultiply.multiplyNaiveSeq(A, B);
            boolean ok = ref.equals(result.result);
            System.out.println("\nVerification vs naive seq: " + (ok ? "OK" : "MISMATCH"));
            if (!ok) {
                System.err.println("First 10 coeffs (ref vs got):");
                for (int i = 0; i < Math.min(10, ref.degree() + 1); i++) {
                    long refVal = ref.getCoeff(i);
                    long gotVal = result.result.getCoeff(i);
                    System.err.println("  k=" + i + "  ref=" + refVal + "  got=" + gotVal);
                }
            }
        }
    }

    private static BenchResult benchmark(String label, Config config, Polynomial A, Polynomial B,
                                         Supplier<Polynomial> fn) {
        List<Double> samples = new ArrayList<>();
        Polynomial result = null;

        for (int r = 0; r < config.repeats; r++) {
            long t0 = System.nanoTime();
            Polynomial tmp = fn.get();
            long t1 = System.nanoTime();
            double ms = (t1 - t0) / 1_000_000.0;
            samples.add(ms);
            if (r == 0) result = tmp;
        }

        double avg = samples.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
        double min = samples.stream().mapToDouble(Double::doubleValue).min().orElse(0.0);

        System.out.printf("%-28s | avg: %9.3f ms  min: %9.3f ms%n", label, avg, min);

        return new BenchResult(result, avg, min);
    }

    private static Config parseArgs(String[] args) {
        Config cfg = new Config();

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];

            switch (arg) {
                case "--algorithm", "-a" -> cfg.algorithm = requireValue(args, ++i, arg);
                case "--mode", "-m" -> cfg.mode = requireValue(args, ++i, arg);
                case "--threads", "-t" -> {
                    int v = Integer.parseInt(requireValue(args, ++i, arg));
                    cfg.threads = Math.max(1, v);
                    cfg.karatsubaMaxDepth = (int) Math.ceil(Math.log(cfg.threads) / Math.log(2));
                }
                case "--size", "-n" -> {
                    long v = Long.parseLong(requireValue(args, ++i, arg));
                    if (v <= 0) throw new IllegalArgumentException("size must be > 0");
                    cfg.size = (int) v;
                }
                case "--repeats", "-r" -> cfg.repeats = Integer.parseInt(requireValue(args, ++i, arg));
                case "--seed" -> cfg.seed = Long.parseLong(requireValue(args, ++i, arg));
                case "--coeff-range" -> {
                    long v = Long.parseLong(requireValue(args, ++i, arg));
                    if (v <= 0) throw new IllegalArgumentException("coeff-range must be > 0");
                    cfg.coeffMin = -v;
                    cfg.coeffMax = v;
                }
                case "--compare" -> cfg.compare = true;
                case "--no-verify" -> cfg.verify = false;
                case "--karatsuba-seq-base" -> cfg.karatsubaSeqBase = Integer.parseInt(requireValue(args, ++i, arg));
                case "--karatsuba-par-base" -> cfg.karatsubaParBase = Integer.parseInt(requireValue(args, ++i, arg));
                case "--karatsuba-max-depth" -> cfg.karatsubaMaxDepth = Integer.parseInt(requireValue(args, ++i, arg));
                case "--help", "-h" -> {
                    printHelp();
                    System.exit(0);
                }
                default -> throw new IllegalArgumentException("Unknown argument: " + arg);
            }
        }

        return cfg;
    }

    private static String requireValue(String[] args, int index, String name) {
        if (index >= args.length) {
            throw new IllegalArgumentException("Missing value for " + name);
        }
        return args[index];
    }

    private static void printHelp() {
        System.out.println("""
            Usage: java -cp build/classes ppd.lab05.Main [OPTIONS]

            Options:
              --algorithm, -a <naive|karatsuba>  Algorithm to use
              --mode, -m <seq|par>               Sequential or parallel mode
              --threads, -t <N>                  Number of threads (default: CPU count)
              --size, -n <N>                     Polynomial size (default: 4000)
              --repeats, -r <R>                  Number of benchmark repeats (default: 3)
              --seed <SEED>                      Random seed (default: current time)
              --coeff-range <V>                  Coefficient range [-V, V] (default: 1000)
              --compare                          Run all 4 variants
              --no-verify                        Skip verification
              --karatsuba-seq-base <B>           Karatsuba sequential base (default: 64)
              --karatsuba-par-base <B>           Karatsuba parallel base (default: 256)
              --karatsuba-max-depth <D>          Karatsuba max parallel depth (default: log2(threads))
              --help, -h                         Show this help

            Examples:
              --compare --size 4000 --threads 8 --repeats 3
              --algorithm naive --mode par --threads 8 --size 10000
            """);
    }

    static class Config {
        String algorithm = "naive";
        String mode = "seq";
        int threads = Runtime.getRuntime().availableProcessors();
        int size = 4000;
        int repeats = 3;
        boolean compare = false;
        boolean verify = true;
        long seed = System.nanoTime();
        long coeffMin = -1000;
        long coeffMax = 1000;
        int karatsubaSeqBase = 64;
        int karatsubaParBase = 256;
        int karatsubaMaxDepth = (int) Math.ceil(Math.log(Runtime.getRuntime().availableProcessors()) / Math.log(2));
    }

    static class BenchResult {
        final Polynomial result;
        final double avgMs;
        final double minMs;

        BenchResult(Polynomial result, double avgMs, double minMs) {
            this.result = result;
            this.avgMs = avgMs;
            this.minMs = minMs;
        }
    }
}
