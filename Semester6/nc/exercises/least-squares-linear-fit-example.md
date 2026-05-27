# Linear Least Squares Approximation — Worked Example

**Problem.** Find the linear polynomial least-squares approximation that best fits the data

$$(0, -2), \quad (1, 1), \quad (2, 3).$$

## First: this is NOT interpolation

> **Interpolation** (Lagrange/Newton): the polynomial passes *exactly* through every point, and its degree is fixed by the number of points (3 points → degree 2).
>
> **Least-squares approximation** (this exercise): you *fix the degree in advance* — here **linear** = degree 1, $\varphi(x) = a + bx$ — and find the coefficients that minimize the total squared error. With 3 points and only 2 free coefficients, the line **cannot** hit all the points; it is the best compromise.

Keywords that signal least squares: "**best fits**", "**least squares**", "**approximation**", or "fit a degree-$k$ polynomial to $N > k+1$ data points". If you used the Newton divided-difference table here you would get the interpolating *quadratic*, which is the wrong object.

## Method — the normal equations (Lecture 8)

We minimize $E^2(\varphi) = \sum_{k} \big(\varphi(x_k) - y_k\big)^2$ over $\varphi(x) = a\,\pi_1(x) + b\,\pi_2(x)$ with basis

$$\pi_1(x) = 1, \qquad \pi_2(x) = x.$$

Setting $\partial E^2/\partial a = \partial E^2/\partial b = 0$ gives the **normal equations** $Ac = B$ with the *discrete* inner product

$$\langle u, v\rangle = \sum_{k} u(x_k)\,v(x_k), \qquad a_{ij} = \langle \pi_i, \pi_j\rangle, \qquad B_i = \langle \pi_i, f\rangle.$$

## Step 1 — tabulate the sums

With the three points $x = (0, 1, 2)$, $y = (-2, 1, 3)$:

| $x_k$ | $y_k$ | $x_k^2$ | $x_k y_k$ |
|------|------|--------|----------|
| $0$  | $-2$ | $0$    | $0$      |
| $1$  | $1$  | $1$    | $1$      |
| $2$  | $3$  | $4$    | $6$      |
| **$\sum = 3$** | **$\sum = 2$** | **$\sum = 5$** | **$\sum = 7$** |

So, with $N = 3$ points:

$$\langle \pi_1, \pi_1\rangle = \sum 1 = N = 3, \qquad \langle \pi_1, \pi_2\rangle = \sum x_k = 3, \qquad \langle \pi_2, \pi_2\rangle = \sum x_k^2 = 5,$$
$$\langle \pi_1, f\rangle = \sum y_k = 2, \qquad \langle \pi_2, f\rangle = \sum x_k y_k = 7.$$

## Step 2 — write the normal equations

$$
\begin{bmatrix} \langle \pi_1,\pi_1\rangle & \langle \pi_1,\pi_2\rangle \\ \langle \pi_2,\pi_1\rangle & \langle \pi_2,\pi_2\rangle \end{bmatrix}
\begin{bmatrix} a \\ b \end{bmatrix}
=
\begin{bmatrix} \langle \pi_1,f\rangle \\ \langle \pi_2,f\rangle \end{bmatrix}
\quad\Longrightarrow\quad
\begin{bmatrix} 3 & 3 \\ 3 & 5 \end{bmatrix}
\begin{bmatrix} a \\ b \end{bmatrix}
=
\begin{bmatrix} 2 \\ 7 \end{bmatrix}.
$$

Written out:

$$
\begin{cases}
3a + 3b = 2 \\
3a + 5b = 7
\end{cases}
$$

## Step 3 — solve

Subtract the first equation from the second:

$$(3a + 5b) - (3a + 3b) = 7 - 2 \ \Rightarrow\ 2b = 5 \ \Rightarrow\ b = \frac{5}{2}.$$

Back-substitute into $3a + 3b = 2$:

$$3a = 2 - 3\cdot\frac{5}{2} = 2 - \frac{15}{2} = -\frac{11}{2} \ \Rightarrow\ a = -\frac{11}{6}.$$

(Equivalently by Cramer's rule, $\det A = 3\cdot 5 - 3\cdot 3 = 6$, $a = \frac{2\cdot 5 - 3\cdot 7}{6} = -\frac{11}{6}$, $b = \frac{3\cdot 7 - 3\cdot 2}{6} = \frac{5}{2}$.)

## Step 4 — the best-fit line

$$\boxed{\,\varphi^*(x) = \frac{5}{2}x - \frac{11}{6}\,}$$

## Step 5 — check the residuals (it should *not* pass through the points)

$$
\begin{array}{c|c|c|c}
x_k & y_k & \varphi^*(x_k) & \text{residual } y_k - \varphi^*(x_k) \\
\hline
0 & -2 & -\tfrac{11}{6} \approx -1.833 & -\tfrac{1}{6} \\
1 & 1 & \tfrac{5}{2} - \tfrac{11}{6} = \tfrac{2}{3} \approx 0.667 & +\tfrac{1}{3} \\
2 & 3 & 5 - \tfrac{11}{6} = \tfrac{19}{6} \approx 3.167 & -\tfrac{1}{6}
\end{array}
$$

The residuals are nonzero (the line misses each point a little) but their signed sum is $-\tfrac16 + \tfrac13 - \tfrac16 = 0$ — a hallmark of the least-squares line: it balances the data rather than threading through it. This is exactly the **data-fitting via normal equations** procedure (Lecture 8, Example 3.5), specialized to a linear model.

## Key takeaway

For "fit a degree-$k$ polynomial that best approximates $N$ points" with $N > k+1$:
1. Pick the basis $\pi_1 = 1, \pi_2 = x, \dots, \pi_{k+1} = x^k$.
2. Build the normal-equations matrix from the sums $\langle \pi_i, \pi_j\rangle = \sum_l x_l^{\,i+j-2}$ and right-hand side $\langle \pi_i, f\rangle = \sum_l x_l^{\,i-1} y_l$.
3. Solve the small symmetric system for the coefficients.

No divided-difference table, no derivative seeding — those belong to Newton/Hermite *interpolation*, which is a different problem.
