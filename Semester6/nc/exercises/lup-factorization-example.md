# LUP Factorization — Worked Example

**Problem.** Find the LUP factorization $PA = LU$ of

$$A = \begin{bmatrix} 2 & 1 & -2 \\ 1 & 1 & -1 \\ 3 & -1 & 1 \end{bmatrix}.$$

## The rule (partial pivoting)

At each column, the **pivot** is the entry of **largest absolute value** in that column on or below the diagonal. Swap that row up (record the swap in $P$), then eliminate the entries below the pivot. Each elimination multiplier is stored in $L$.

- $L$ is unit lower triangular (1's on the diagonal), holding the multipliers.
- $U$ is upper triangular (the result of elimination).
- $P$ is the permutation matrix recording the row swaps, so that $PA = LU$.

## Stage 1 — column 1

Column-1 entries: $\{2, 1, 3\}$. Largest in absolute value is $\mathbf{3}$ (row 3) → **swap $R_1 \leftrightarrow R_3$**:

$$\begin{bmatrix} 3 & -1 & 1 \\ 1 & 1 & -1 \\ 2 & 1 & -2 \end{bmatrix}$$

Eliminate below the pivot $3$:

- $m_{21} = \dfrac13$: $\ R_2 \leftarrow R_2 - \tfrac13 R_1 = [\,0,\ \tfrac43,\ -\tfrac43\,]$
- $m_{31} = \dfrac23$: $\ R_3 \leftarrow R_3 - \tfrac23 R_1 = [\,0,\ \tfrac53,\ -\tfrac83\,]$

$$\begin{bmatrix} 3 & -1 & 1 \\ 0 & \tfrac43 & -\tfrac43 \\ 0 & \tfrac53 & -\tfrac83 \end{bmatrix}$$

## Stage 2 — column 2

Entries on/below the diagonal in column 2: $\{\tfrac43,\ \tfrac53\}$. Largest is $\mathbf{\tfrac53}$ (row 3) → **swap $R_2 \leftrightarrow R_3$**.

> **Key point.** This swap also moves the **already-stored multipliers** in column 1: the $\tfrac23$ (currently in row 3) rises to position $(2,1)$ and the $\tfrac13$ drops to $(3,1)$. The multipliers ride along with their rows.

After the swap:

$$\begin{bmatrix} 3 & -1 & 1 \\ 0 & \tfrac53 & -\tfrac83 \\ 0 & \tfrac43 & -\tfrac43 \end{bmatrix}$$

Eliminate below the pivot $\tfrac53$:

- $m_{32} = \dfrac{4/3}{5/3} = \dfrac45$: $\ R_3 \leftarrow R_3 - \tfrac45 R_2$
  - column 3: $-\tfrac43 - \tfrac45\left(-\tfrac83\right) = -\tfrac{20}{15} + \tfrac{32}{15} = \tfrac{12}{15} = \tfrac45$

$$U = \begin{bmatrix} 3 & -1 & 1 \\ 0 & \tfrac53 & -\tfrac83 \\ 0 & 0 & \tfrac45 \end{bmatrix}$$

## Assemble $L$, $U$, $P$

$$L = \begin{bmatrix} 1 & 0 & 0 \\ \tfrac23 & 1 & 0 \\ \tfrac13 & \tfrac45 & 1 \end{bmatrix}, \qquad U = \begin{bmatrix} 3 & -1 & 1 \\ 0 & \tfrac53 & -\tfrac83 \\ 0 & 0 & \tfrac45 \end{bmatrix}.$$

For $P$, track the net row order. The swaps $R_1 \leftrightarrow R_3$ then $R_2 \leftrightarrow R_3$ send the original row order $(1,2,3) \to (3,2,1) \to (3,1,2)$. So $PA$ takes the original rows in the order $(3, 1, 2)$:

$$P = \begin{bmatrix} 0 & 0 & 1 \\ 1 & 0 & 0 \\ 0 & 1 & 0 \end{bmatrix}.$$

## Verify $PA = LU$

$$PA = \begin{bmatrix} 3 & -1 & 1 \\ 2 & 1 & -2 \\ 1 & 1 & -1 \end{bmatrix}.$$

Check the product $LU$:

- Row 1: $1\cdot[3,-1,1] = [3,-1,1]$ ✓
- Row 2: $\tfrac23[3,-1,1] + [0,\tfrac53,-\tfrac83] = [2,\ 1,\ -2]$ ✓
- Row 3: $\tfrac13[3,-1,1] + \tfrac45[0,\tfrac53,-\tfrac83] + [0,0,\tfrac45] = [1,\ 1,\ -1]$ ✓

So $LU = PA$.

## Using the factorization to solve $Ax = b$

Once you have $PA = LU$, solving $Ax = b$ is two triangular solves on the **permuted** right-hand side $Pb$:

1. Forward-solve $Ly = Pb$ (lower triangular, top-down).
2. Back-solve $Ux = y$ (upper triangular, bottom-up).

### Full worked solve

Take $b = \begin{bmatrix} -3 \\ 1 \\ 3 \end{bmatrix}$. (Chosen so both solves stay fraction-free: the last back-substitution step is $\tfrac45 x_3 = y_3$, so a clean $x_3$ needs $y_3$ a multiple of $\tfrac45$ — this $b$ arranges that.)

**Permute the right-hand side.** $P$ takes the rows in order $(3,1,2)$, so

$$Pb = \begin{bmatrix} b_3 \\ b_1 \\ b_2 \end{bmatrix} = \begin{bmatrix} 3 \\ -3 \\ 1 \end{bmatrix}.$$

**Forward-solve $Ly = Pb$** (top → down), with $L = \begin{bmatrix} 1 & 0 & 0 \\ \tfrac23 & 1 & 0 \\ \tfrac13 & \tfrac45 & 1 \end{bmatrix}$:

$$y_1 = 3,$$
$$\tfrac23 y_1 + y_2 = -3 \;\Rightarrow\; y_2 = -3 - \tfrac23(3) = -3 - 2 = -5,$$
$$\tfrac13 y_1 + \tfrac45 y_2 + y_3 = 1 \;\Rightarrow\; y_3 = 1 - \tfrac13(3) - \tfrac45(-5) = 1 - 1 + 4 = 4.$$

So $y = (3,\ -5,\ 4)^T$.

**Back-solve $Ux = y$** (bottom → up), with $U = \begin{bmatrix} 3 & -1 & 1 \\ 0 & \tfrac53 & -\tfrac83 \\ 0 & 0 & \tfrac45 \end{bmatrix}$:

$$\tfrac45 x_3 = 4 \;\Rightarrow\; x_3 = 5,$$
$$\tfrac53 x_2 - \tfrac83 x_3 = -5 \;\Rightarrow\; \tfrac53 x_2 = -5 + \tfrac83(5) = \tfrac{-15 + 40}{3} = \tfrac{25}{3} \;\Rightarrow\; x_2 = \tfrac{25}{3}\cdot\tfrac{3}{5} = 5,$$
$$3x_1 - x_2 + x_3 = 3 \;\Rightarrow\; 3x_1 = 3 + 5 - 5 = 3 \;\Rightarrow\; x_1 = 1.$$

$$\boxed{\,x = (1,\ 5,\ 5)^T\,}$$

**Verify** against the original $Ax = b$:

- $2(1) + 1(5) - 2(5) = 2 + 5 - 10 = -3$ ✓
- $1(1) + 1(5) - 1(5) = 1 + 5 - 5 = 1$ ✓
- $3(1) - 1(5) + 1(5) = 3 - 5 + 5 = 3$ ✓

## Key takeaway

1. Pick the largest-magnitude pivot in the current column (partial pivoting) — this keeps the multipliers $\le 1$ in size and the method numerically stable.
2. Eliminate below the pivot; store each multiplier in $L$.
3. **When a later pivot forces a row swap, the multipliers already stored in earlier columns swap with their rows.** This is the most common mistake.
4. $P$ records the cumulative row order; verify with $PA = LU$.
5. To solve a system, permute $b$ first ($Pb$), then forward/back-substitute.
