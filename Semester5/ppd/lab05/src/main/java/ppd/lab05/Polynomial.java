package ppd.lab05;

import java.util.*;

/**
 * Represents a polynomial with long coefficients.
 * Coefficients are stored in increasing degree order: [a0, a1, a2, ...] represents a0 + a1*x + a2*x^2 + ...
 */
public class Polynomial {
    private final long[] coeffs;
    private final int degree;
    
    /**
     * Creates a polynomial from an array of coefficients.
     * Trailing zeros are trimmed to maintain correct degree.
     * 
     * @param coeffs coefficient array in increasing degree order
     */
    public Polynomial(long[] coeffs) {
        if (coeffs == null || coeffs.length == 0) {
            this.coeffs = new long[]{0};
            this.degree = 0;
        } else {
            // Trim trailing zeros
            int lastNonZero = coeffs.length - 1;
            while (lastNonZero > 0 && coeffs[lastNonZero] == 0) {
                lastNonZero--;
            }
            this.coeffs = Arrays.copyOf(coeffs, lastNonZero + 1);
            this.degree = lastNonZero;
        }
    }
    
    /**
     * Creates a polynomial from a list of coefficients.
     */
    public Polynomial(List<Long> coeffList) {
        this(coeffList.stream().mapToLong(Long::longValue).toArray());
    }
    
    /**
     * Returns the degree of the polynomial.
     */
    public int degree() {
        return degree;
    }
    
    /**
     * Returns the number of coefficients (degree + 1).
     */
    public int size() {
        return coeffs.length;
    }
    
    /**
     * Returns the coefficient at the given index.
     * Returns 0 if index is out of bounds.
     */
    public long getCoeff(int index) {
        if (index < 0 || index >= coeffs.length) {
            return 0;
        }
        return coeffs[index];
    }
    
    /**
     * Returns a copy of the coefficients array.
     */
    public long[] getCoeffs() {
        return Arrays.copyOf(coeffs, coeffs.length);
    }
    
    /**
     * Returns a copy of coefficients padded to the specified size with zeros.
     */
    public long[] getCoeffsPadded(int size) {
        return Arrays.copyOf(coeffs, size);
    }
    
    /**
     * Adds two polynomials.
     */
    public static Polynomial add(Polynomial a, Polynomial b) {
        int maxSize = Math.max(a.size(), b.size());
        long[] result = new long[maxSize];
        
        for (int i = 0; i < maxSize; i++) {
            result[i] = a.getCoeff(i) + b.getCoeff(i);
        }
        
        return new Polynomial(result);
    }
    
    /**
     * Subtracts polynomial b from polynomial a.
     */
    public static Polynomial subtract(Polynomial a, Polynomial b) {
        int maxSize = Math.max(a.size(), b.size());
        long[] result = new long[maxSize];
        
        for (int i = 0; i < maxSize; i++) {
            result[i] = a.getCoeff(i) - b.getCoeff(i);
        }
        
        return new Polynomial(result);
    }
    
    /**
     * Shifts the polynomial left by k positions (multiplies by x^k).
     */
    public static Polynomial shiftLeft(Polynomial p, int k) {
        if (k <= 0) return p;
        
        long[] result = new long[p.size() + k];
        System.arraycopy(p.coeffs, 0, result, k, p.size());
        
        return new Polynomial(result);
    }
    
    /**
     * Generates a random polynomial with coefficients in the range [min, max].
     */
    public static Polynomial random(int size, long min, long max, Random rng) {
        if (size <= 0) size = 1;
        
        long[] coeffs = new long[size];
        for (int i = 0; i < size; i++) {
            // Generate random long in range [min, max]
            long range = max - min + 1;
            coeffs[i] = min + (long) (rng.nextDouble() * range);
        }
        
        return new Polynomial(coeffs);
    }
    
    /**
     * Generates a polynomial with all coefficients set to 1.
     */
    public static Polynomial allOnes(int size) {
        if (size <= 0) size = 1;
        
        long[] coeffs = new long[size];
        for (int i = 0; i < size; i++) {
            coeffs[i] = 1;
        }
        
        return new Polynomial(coeffs);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof Polynomial)) return false;
        
        Polynomial other = (Polynomial) obj;
        return Arrays.equals(this.coeffs, other.coeffs);
    }
    
    @Override
    public int hashCode() {
        return Arrays.hashCode(coeffs);
    }
    
    @Override
    public String toString() {
        if (degree == 0) {
            return String.valueOf(coeffs[0]);
        }
        
        StringBuilder sb = new StringBuilder();
        for (int i = degree; i >= 0; i--) {
            long c = coeffs[i];
            if (c == 0) continue;
            
            if (sb.length() > 0 && c > 0) {
                sb.append(" + ");
            } else if (c < 0) {
                sb.append(" - ");
                c = -c;
            }
            
            if (i == 0) {
                sb.append(c);
            } else if (i == 1) {
                if (c == 1) sb.append("x");
                else sb.append(c).append("x");
            } else {
                if (c == 1) sb.append("x^").append(i);
                else sb.append(c).append("x^").append(i);
            }
        }
        
        return sb.toString();
    }
    
    /**
     * Returns a compact string representation showing only the first few and last few coefficients.
     */
    public String toCompactString(int show) {
        if (coeffs.length <= 2 * show) {
            return Arrays.toString(coeffs);
        }
        
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < show; i++) {
            sb.append(coeffs[i]);
            if (i < show - 1) sb.append(", ");
        }
        sb.append(", ... (").append(coeffs.length - 2 * show).append(" more), ");
        for (int i = coeffs.length - show; i < coeffs.length; i++) {
            sb.append(coeffs[i]);
            if (i < coeffs.length - 1) sb.append(", ");
        }
        sb.append("]");
        
        return sb.toString();
    }
}