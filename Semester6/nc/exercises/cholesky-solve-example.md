# Solving a Linear System with Cholesky Factorization — Worked Example

**Problem.** Use Cholesky factorization to solve $Ax = b$, where

$$A = \begin{bmatrix} 4 & 2 & 6 \\ 2 & 2 & 5 \\ 6 & 5 & 22 \end{bmatrix}, \qquad b = \begin{bmatrix} -2 \\ 0 \\ 1 \end{bmatrix}.$$

## Step 0 — check that Cholesky applies

Cholesky ($A = R^T R$, $R$ upper triangular with $r_{ii} > 0$) needs $A$ **symmetric positive definite**.

- **Symmetric:** $a_{ij} = a_{ji}$ ✓
- **Positive definite** (Sylvester — all leading principal minors $> 0$):
$$\Delta_1 = 4 > 0, \quad \Delta_2 = \det\begin{bmatrix} 4 & 2 \\ 2 & 2 \end{bmatrix} = 4 > 0, \quad \Delta_3 = \det A = 36 > 0. \ \checkmark$$

So Theorem 1.8 applies and a unique Cholesky factor exists.

## Step 1 — factor via row elimination ($A = LDL^T$, then $R = \sqrt{D}\,L^T$)

Do symmetric Gaussian elimination on $A$ (no pivoting needed for SPD). Record each multiplier $\ell_{ij}$ and the diagonal pivots that appear.

- $\ell_{21} = \dfrac{2}{4} = \dfrac12,\quad R_2 \leftarrow R_2 - \tfrac12 R_1$
- $\ell_{31} = \dfrac{6}{4} = \dfrac32,\quad R_3 \leftarrow R_3 - \tfrac32 R_1$

$$\begin{bmatrix} 4 & 2 & 6 \\ 2 & 2 & 5 \\ 6 & 5 & 22 \end{bmatrix} \longrightarrow \begin{bmatrix} 4 & 2 & 6 \\ 0 & 1 & 2 \\ 0 & 2 & 13 \end{bmatrix}$$

- $\ell_{32} = \dfrac{2}{1} = 2,\quad R_3 \leftarrow R_3 - 2 R_2$

$$\longrightarrow \begin{bmatrix} 4 & 2 & 6 \\ 0 & 1 & 2 \\ 0 & 0 & 9 \end{bmatrix}.$$

The diagonal pivots give $D$ and the multipliers give the unit-lower $L$:

$$D = \begin{bmatrix} 4 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 9 \end{bmatrix}, \qquad L = \begin{bmatrix} 1 & 0 & 0 \\ \tfrac12 & 1 & 0 \\ \tfrac32 & 2 & 1 \end{bmatrix}, \qquad \text{so} \quad A = L D L^T.$$

Written out in full:

$$A = L D L^T = \begin{bmatrix} 1 & 0 & 0 \\ \tfrac12 & 1 & 0 \\ \tfrac32 & 2 & 1 \end{bmatrix} \begin{bmatrix} 4 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 9 \end{bmatrix} \begin{bmatrix} 1 & \tfrac12 & \tfrac32 \\ 0 & 1 & 2 \\ 0 & 0 & 1 \end{bmatrix}.$$

Now split $D = \sqrt{D}\,\sqrt{D}$ with $\sqrt{D} = \mathrm{diag}(2, 1, 3)$, and absorb one $\sqrt{D}$ into each side:

$$A = L\,\sqrt{D}\,\sqrt{D}\,L^T = \big(L\sqrt{D}\big)\big(\sqrt{D}\,L^T\big) = R^T R.$$

**Form $R^T$** by multiplying $L$ on the right by $\sqrt{D}$ (this scales the columns of $L$ by $2, 1, 3$):

$$R^T = L\sqrt{D} = \begin{bmatrix} 1 & 0 & 0 \\ \tfrac12 & 1 & 0 \\ \tfrac32 & 2 & 1 \end{bmatrix} \begin{bmatrix} 2 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 3 \end{bmatrix} = \begin{bmatrix} 2 & 0 & 0 \\ 1 & 1 & 0 \\ 3 & 2 & 3 \end{bmatrix}.$$

**Form $R$** by multiplying $L^T$ on the left by $\sqrt{D}$ (this scales the rows of $L^T$ by $2, 1, 3$):

$$R = \sqrt{D}\,L^T = \begin{bmatrix} 2 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 3 \end{bmatrix} \begin{bmatrix} 1 & \tfrac12 & \tfrac32 \\ 0 & 1 & 2 \\ 0 & 0 & 1 \end{bmatrix} = \begin{bmatrix} 2 & 1 & 3 \\ 0 & 1 & 2 \\ 0 & 0 & 3 \end{bmatrix}.$$

So $A = R^T R$ with

$$R^T = \begin{bmatrix} 2 & 0 & 0 \\ 1 & 1 & 0 \\ 3 & 2 & 3 \end{bmatrix}, \qquad R = \begin{bmatrix} 2 & 1 & 3 \\ 0 & 1 & 2 \\ 0 & 0 & 3 \end{bmatrix}.$$

(Notice the pivots $4, 1, 9$ are exactly the squares of $R$'s diagonal — that is why $\sqrt{D}$ recovers it.)

## Step 2 — forward substitution: solve $R^T y = b$

With $R^T = \begin{bmatrix} 2 & 0 & 0 \\ 1 & 1 & 0 \\ 3 & 2 & 3 \end{bmatrix}$ and $b = (-2, 0, 1)^T$:

$$2 y_1 = -2 \ \Rightarrow\ y_1 = -1,$$
$$y_1 + y_2 = 0 \ \Rightarrow\ y_2 = 1,$$
$$3 y_1 + 2 y_2 + 3 y_3 = 1 \ \Rightarrow\ -3 + 2 + 3 y_3 = 1 \ \Rightarrow\ y_3 = \frac{2}{3}.$$

So $y = \left(-1,\ 1,\ \tfrac{2}{3}\right)^T$.

## Step 3 — backward substitution: solve $R x = y$

With $R = \begin{bmatrix} 2 & 1 & 3 \\ 0 & 1 & 2 \\ 0 & 0 & 3 \end{bmatrix}$:

$$3 x_3 = \frac{2}{3} \ \Rightarrow\ x_3 = \frac{2}{9},$$
$$x_2 + 2 x_3 = 1 \ \Rightarrow\ x_2 = 1 - \frac{4}{9} = \frac{5}{9},$$
$$2 x_1 + x_2 + 3 x_3 = -1 \ \Rightarrow\ 2 x_1 = -1 - \frac{5}{9} - \frac{6}{9} = -\frac{20}{9} \ \Rightarrow\ x_1 = -\frac{10}{9}.$$

## Solution

$$\boxed{\,x = \begin{bmatrix} -\tfrac{10}{9} \\[2pt] \tfrac{5}{9} \\[2pt] \tfrac{2}{9} \end{bmatrix} \approx \begin{bmatrix} -1.111 \\ 0.556 \\ 0.222 \end{bmatrix}}$$

## Verification (plug into the original $Ax = b$)

$$
\begin{aligned}
4(-\tfrac{10}{9}) + 2(\tfrac{5}{9}) + 6(\tfrac{2}{9}) &= \tfrac{-40 + 10 + 12}{9} = \tfrac{-18}{9} = -2 \ \checkmark \\
2(-\tfrac{10}{9}) + 2(\tfrac{5}{9}) + 5(\tfrac{2}{9}) &= \tfrac{-20 + 10 + 10}{9} = 0 \ \checkmark \\
6(-\tfrac{10}{9}) + 5(\tfrac{5}{9}) + 22(\tfrac{2}{9}) &= \tfrac{-60 + 25 + 44}{9} = \tfrac{9}{9} = 1 \ \checkmark
\end{aligned}
$$

## Key takeaway

The recipe via $LDL^T$:
1. Confirm $A$ is symmetric positive definite (Sylvester / no negative pivot).
2. Row-eliminate $A$ to read off the **pivots** $D$ and the **multipliers** $L$, so $A = LDL^T$.
3. Build the Cholesky factor $R = \sqrt{D}\,L^T$ (equivalently $R^T = L\sqrt{D}$), giving $A = R^T R$.
4. Forward-solve $R^T y = b$, then back-solve $R x = y$.

If a pivot comes out $\le 0$, the matrix is not positive definite and Cholesky does not exist (real) — fall back to LUP. Here the original $a_{33} = 2$ gave a negative pivot ($\sqrt{-11}$); the correct $a_{33} = 22$ gives pivot $9$, i.e. $r_{33} = 3$.
