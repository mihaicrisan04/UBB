# Gaussian Elimination — Worked Example

**Problem.** Solve $Ax = b$ by the Gaussian elimination method:

$$\begin{cases} x_1 + 2x_2 + x_3 = 1 \\ 2x_1 + 5x_2 + 3x_3 = 3 \\ x_1 + 3x_2 + 4x_3 = 6 \end{cases}$$

## Method

The most basic direct solver, in two phases:

1. **Forward elimination** — use row operations to zero everything below the diagonal, turning $[A\,|\,b]$ into an upper-triangular system. At each step the multiplier is $m_{ij} = \dfrac{a_{ij}}{a_{jj}}$ (pivot $a_{jj}$ = current diagonal entry), then $R_i \leftarrow R_i - m_{ij} R_j$.
2. **Back substitution** — solve from the bottom row upward.

Same arithmetic as LU factorization, applied straight to the augmented matrix without storing the multipliers. No row swaps are needed here since every pivot is nonzero (otherwise: partial pivoting → LUP).

## Forward elimination

Augmented matrix:

$$[A\,|\,b] = \left[\begin{array}{ccc|c} 1 & 2 & 1 & 1 \\ 2 & 5 & 3 & 3 \\ 1 & 3 & 4 & 6 \end{array}\right]$$

**Column 1** — pivot $a_{11} = 1$. Multipliers $m_{21} = \tfrac21 = 2$, $m_{31} = \tfrac11 = 1$:

- $R_2 \leftarrow R_2 - 2R_1 = [\,0,\ 1,\ 1\ |\ 1\,]$
- $R_3 \leftarrow R_3 - 1R_1 = [\,0,\ 1,\ 3\ |\ 5\,]$

$$\left[\begin{array}{ccc|c} 1 & 2 & 1 & 1 \\ 0 & 1 & 1 & 1 \\ 0 & 1 & 3 & 5 \end{array}\right]$$

**Column 2** — pivot $a_{22} = 1$. Multiplier $m_{32} = \tfrac11 = 1$:

- $R_3 \leftarrow R_3 - 1R_2 = [\,0,\ 0,\ 2\ |\ 4\,]$

$$U = \left[\begin{array}{ccc|c} 1 & 2 & 1 & 1 \\ 0 & 1 & 1 & 1 \\ 0 & 0 & 2 & 4 \end{array}\right]$$

The system is now upper-triangular.

## Back substitution

From the bottom row up:

$$2x_3 = 4 \;\Rightarrow\; x_3 = 2,$$
$$x_2 + x_3 = 1 \;\Rightarrow\; x_2 = 1 - 2 = -1,$$
$$x_1 + 2x_2 + x_3 = 1 \;\Rightarrow\; x_1 = 1 - 2(-1) - 2 = 1.$$

$$\boxed{\,x = (1,\ -1,\ 2)^T\,}$$

## Verification (plug into the original system)

- $1 + 2(-1) + 2 = 1$ ✓
- $2(1) + 5(-1) + 3(2) = 2 - 5 + 6 = 3$ ✓
- $1 + 3(-1) + 4(2) = 1 - 3 + 8 = 6$ ✓

## Key takeaway

1. Work on the augmented matrix $[A\,|\,b]$.
2. Forward-eliminate column by column: multiplier $m_{ij} = a_{ij}/a_{jj}$, then $R_i \leftarrow R_i - m_{ij}R_j$, until upper-triangular.
3. Back-substitute from the last equation up.
4. If a pivot is zero (or for numerical stability), swap in the largest-magnitude entry from below — that is partial pivoting, which turns this into the LUP method.
