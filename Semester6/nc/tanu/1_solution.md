# Solution — Numerical Calculus Exam, 1

---

## Problem 1

> Let $f : [-1, 1] \to \mathbb{R}$ be twice continuously differentiable.
>
> a) (1 point) Find $(Pf)(x)$, the polynomial interpolating the function $f$ at the simple nodes $x_0 = -1$ and $x_1 = 1$.
>
> b) (1 point) Integrate $\int_{-1}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{-1}^{1}f(x)\,dx$. Find its degree of precision.
>
> c) (1.5 points) Use Peano's theorem to find the remainder $R(f)$ of the quadrature formula in b).

### a) Find $(Pf)(x)$

**Formula used — Lagrange interpolation polynomial at $n+1$ simple nodes:**

$$L_n f(x) = \sum_{i=0}^{n} l_i(x) f(x_i), \quad l_i(x) = \prod_{\substack{j=0 \\ j \neq i}}^{n} \frac{x - x_j}{x_i - x_j}$$

Here $n = 1$, $x_0 = -1$, $x_1 = 1$:

$$l_0(x) = \frac{x - x_1}{x_0 - x_1} = \frac{x - 1}{-1 - 1} = \frac{1 - x}{2}, \quad l_1(x) = \frac{x - x_0}{x_1 - x_0} = \frac{x + 1}{1 + 1} = \frac{x + 1}{2}$$

$$\boxed{(Pf)(x) = \frac{1 - x}{2} f(-1) + \frac{1 + x}{2} f(1)}$$

### b) Quadrature formula and degree of precision

**Formula used — quadrature by integrating the interpolation polynomial:**

$$\int_a^b f(x)\,dx \approx \int_a^b (Pf)(x)\,dx = \sum_{i=0}^{n} A_i f(x_i), \quad A_i = \int_a^b l_i(x)\,dx$$

**Degree of precision:** the largest $d$ such that $R(e_k) = 0$ for all $k = 0, 1, \ldots, d$, where $e_k(x) = x^k$ and $R(f) = \int_a^b f(x)\,dx - \sum A_i f(x_i)$.

Compute the weights:

$$A_0 = \int_{-1}^{1} \frac{1 - x}{2}\,dx$$

*Integral reminder:* $\int \frac{1-x}{2}\,dx = \frac{1}{2}\int(1-x)\,dx$. The antiderivative of $1$ is $x$. The antiderivative of $x$ is $\frac{x^2}{2}$. So:

$$\int \frac{1-x}{2}\,dx = \frac{1}{2}\left(x - \frac{x^2}{2}\right)$$

Now evaluate at the bounds:

$$A_0 = \frac{1}{2}\left(x - \frac{x^2}{2}\right)\Bigg|_{-1}^{1} = \frac{1}{2}\left[\left(1 - \frac{1}{2}\right) - \left(-1 - \frac{1}{2}\right)\right] = \frac{1}{2}\left[\frac{1}{2} - \left(-\frac{3}{2}\right)\right] = \frac{1}{2} \cdot \frac{4}{2} = \frac{1}{2} \cdot 2 = 1$$

$$A_1 = \int_{-1}^{1} \frac{1 + x}{2}\,dx$$

$$\int \frac{1+x}{2}\,dx = \frac{1}{2}\left(x + \frac{x^2}{2}\right)$$

$$A_1 = \frac{1}{2}\left(x + \frac{x^2}{2}\right)\Bigg|_{-1}^{1} = \frac{1}{2}\left[\left(1 + \frac{1}{2}\right) - \left(-1 + \frac{1}{2}\right)\right] = \frac{1}{2}\left[\frac{3}{2} - \left(-\frac{1}{2}\right)\right] = \frac{1}{2} \cdot \frac{4}{2} = 1$$

So the quadrature formula is:

$$\boxed{\int_{-1}^{1} f(x)\,dx \approx 1 \cdot f(-1) + 1 \cdot f(1) = f(-1) + f(1)}$$

This is the **trapezoidal rule** on $[-1, 1]$.

**Degree of precision:**

*Integral reminder:* $\int_{-1}^{1} x^k\,dx$: the antiderivative of $x^k$ is $\frac{x^{k+1}}{k+1}$.

- $R(e_0)$: $\int_{-1}^{1} 1\,dx = x\Big|_{-1}^{1} = 1 - (-1) = 2$. Quadrature: $f(-1) + f(1) = 1 + 1 = 2$. So $R(e_0) = 2 - 2 = 0$ $\checkmark$

- $R(e_1)$: $\int_{-1}^{1} x\,dx = \frac{x^2}{2}\Big|_{-1}^{1} = \frac{1}{2} - \frac{1}{2} = 0$. Quadrature: $(-1) + 1 = 0$. So $R(e_1) = 0 - 0 = 0$ $\checkmark$

- $R(e_2)$: $\int_{-1}^{1} x^2\,dx = \frac{x^3}{3}\Big|_{-1}^{1} = \frac{1}{3} - \frac{-1}{3} = \frac{1}{3} + \frac{1}{3} = \frac{2}{3}$. Quadrature: $(-1)^2 + 1^2 = 1 + 1 = 2$. So $R(e_2) = \frac{2}{3} - 2 = -\frac{4}{3} \neq 0$

The degree of precision is $d = 1$.

### c) Remainder using Peano's theorem

**Formula used — Peano's theorem:** Let $L$ be a linear functional on $H^{n+1}[a,b]$ with $\ker L = \mathbb{P}_n$ (i.e., degree of precision $d = n$). Then:

$$Lf = \int_a^b K_n(t)\, f^{(n+1)}(t)\,dt$$

where the **Peano kernel** is $K_n(t) = \frac{1}{n!} L_x\big((x - t)_+^n\big)$.

**Corollary:** If $K_n$ has constant sign on $[a, b]$, then:

$$Lf = \frac{1}{(n+1)!} f^{(n+1)}(\xi) \cdot L(e_{n+1}), \quad \xi \in (a, b)$$

Here $L = R$ (the remainder), $n = 1$ (degree of precision $d = 1$), $[a, b] = [-1, 1]$.

$$R(f) = \int_{-1}^{1} f(x)\,dx - f(-1) - f(1)$$

The Peano kernel is:

$$K_1(t) = R_x\big((x - t)_+\big) = \int_{-1}^{1} (x - t)_+\,dx - (-1 - t)_+ - (1 - t)_+$$

*Recall:* $(z)_+ = \max(z, 0)$, i.e., $z$ if $z \geq 0$, and $0$ if $z < 0$.

For $t \in [-1, 1]$:
- $(-1 - t)_+$: since $t \geq -1$, we have $-1 - t \leq 0$, so $(-1 - t)_+ = 0$
- $(1 - t)_+$: since $t \leq 1$, we have $1 - t \geq 0$, so $(1 - t)_+ = 1 - t$

For the integral, $(x - t)_+ = 0$ when $x < t$ and $(x-t)_+ = x - t$ when $x \geq t$:

$$\int_{-1}^{1} (x - t)_+\,dx = \int_{-1}^{t} 0\,dx + \int_{t}^{1} (x - t)\,dx = \int_{t}^{1} (x - t)\,dx$$

*Computing this integral step by step:*

The antiderivative of $(x - t)$ with respect to $x$ is $\frac{(x-t)^2}{2}$ (since $t$ is a constant here).

$$\int_{t}^{1} (x - t)\,dx = \frac{(x - t)^2}{2}\Bigg|_{t}^{1} = \frac{(1 - t)^2}{2} - \frac{(t - t)^2}{2} = \frac{(1 - t)^2}{2} - 0 = \frac{(1 - t)^2}{2}$$

Putting it all together:

$$K_1(t) = \frac{(1 - t)^2}{2} - 0 - (1 - t) = \frac{(1 - t)^2}{2} - (1 - t)$$

Factor out $(1-t)$:

$$K_1(t) = (1-t)\left(\frac{1-t}{2} - 1\right) = (1-t) \cdot \frac{1 - t - 2}{2} = (1-t) \cdot \frac{-(1+t)}{2} = -\frac{(1-t)(1+t)}{2}$$

$$K_1(t) = -\frac{1 - t^2}{2} \leq 0, \quad \forall t \in [-1, 1]$$

(This is $\leq 0$ because $1 - t^2 \geq 0$ for $t \in [-1, 1]$, so the whole expression is $\leq 0$.)

Since $K_1(t)$ has **constant sign** (non-positive) on $[-1, 1]$, the Corollary applies:

$$R(f) = \frac{1}{(n+1)!} f^{(n+1)}(\xi) \cdot R(e_{n+1}) = \frac{1}{2!} f''(\xi) \cdot R(e_2), \quad \xi \in (-1, 1)$$

We already computed $R(e_2) = -\frac{4}{3}$, so:

$$R(f) = \frac{1}{2} \cdot f''(\xi) \cdot \left(-\frac{4}{3}\right) = -\frac{4}{6} f''(\xi)$$

$$\boxed{R(f) = -\frac{2}{3} f''(\xi), \quad \xi \in (-1, 1)}$$

---

## Problem 2

> (2.5 points) Show that the polynomial $2x^5 - 5x^4 + 10x^2 - 11x + 4$ has $\alpha = 1$ as a root. Does the iteration $x_{n+1} = 2x_n^5 - 5x_n^4 + 10x_n^2 - 10x_n + 4$ converge to $\alpha$ for a suitable $x_0$? Study the convergence of the method, find the order of convergence and give a bound for the error.

**Formula used — convergence of one-point iteration $x_{n+1} = g(x_n)$ (Theorem 3.6):**

If $\alpha$ is a fixed point of $g$ and $|g'(\alpha)| < 1$, then the method converges for $x_0$ sufficiently close to $\alpha$.

**Order of convergence:** If $g'(\alpha) = g''(\alpha) = \cdots = g^{(p-1)}(\alpha) = 0$ and $g^{(p)}(\alpha) \neq 0$, then the method has order $p$, with:

$$\lim_{n \to \infty} \frac{|x_{n+1} - \alpha|}{|x_n - \alpha|^p} = \frac{|g^{(p)}(\alpha)|}{p!}$$

**Error bound (Banach/contraction, Theorem 3.3):** $|x_n - \alpha| \leq \lambda^n |x_0 - \alpha|$, where $\lambda = \max_{x \in [a,b]} |g'(x)| < 1$.

---

Let $p(x) = 2x^5 - 5x^4 + 10x^2 - 11x + 4$.

**Show $\alpha = 1$ is a root:**

$$p(1) = 2(1) - 5(1) + 10(1) - 11(1) + 4 = 2 - 5 + 10 - 11 + 4 = 0 \checkmark$$

**Fixed-point iteration:** $g(x) = 2x^5 - 5x^4 + 10x^2 - 10x + 4$, so $x_{n+1} = g(x_n)$.

**Check fixed point:** $g(1) = 2 - 5 + 10 - 10 + 4 = 1$ $\checkmark$

Note: $g(x) = p(x) + x$, so $p(\alpha) = 0 \iff g(\alpha) = \alpha$.

**Compute derivatives of $g$ at $\alpha = 1$:**

*Derivative reminder:* $(x^n)' = nx^{n-1}$

$$g(x) = 2x^5 - 5x^4 + 10x^2 - 10x + 4$$

$$g'(x) = 10x^4 - 20x^3 + 20x - 10 \implies g'(1) = 10 - 20 + 20 - 10 = 0$$

$$g''(x) = 40x^3 - 60x^2 + 20 \implies g''(1) = 40 - 60 + 20 = 0$$

$$g'''(x) = 120x^2 - 120x \implies g'''(1) = 120 - 120 = 0$$

$$g^{(4)}(x) = 240x - 120 \implies g^{(4)}(1) = 240 - 120 = 120 \neq 0$$

Since $g'(1) = g''(1) = g'''(1) = 0$ and $g^{(4)}(1) = 120 \neq 0$:

- The method **converges** for $x_0$ sufficiently close to $\alpha = 1$ (since $|g'(1)| = 0 < 1$).
- The **order of convergence is $p = 4$**.

**Error bound:**

$$\boxed{|x_{n+1} - 1| \approx \frac{|g^{(4)}(1)|}{4!} |x_n - 1|^4 = \frac{120}{24} |x_n - 1|^4 = 5 |x_n - 1|^4}$$

---

## Problem 3

> Let $A = \begin{bmatrix} 2 & 4 & 2 \\ 4 & -10 & 2 \\ 1 & 2 & 4 \end{bmatrix}$.
>
> a) (1.5 points) Find the LUP decomposition with partial pivoting.
>
> b) (1.5 points) Use it to solve the system $Ax = [6\ -10\ 9]^T$.

### a) LUP decomposition with partial pivoting

**Formula used — LUP factorization:** Find $L$ (lower triangular, 1s on diagonal), $U$ (upper triangular), $P$ (permutation matrix) such that:

$$PA = LU$$

**Partial pivoting:** at each step $k$, choose the pivot as the largest element (in absolute value) in column $k$, from row $k$ downward: $|a_{pk}^{(k)}| = \max_{k \leq l \leq n} |a_{lk}^{(k)}|$. Swap rows $R_k \leftrightarrow R_p$.

**Multipliers:** $m_{i,k} = a_{ik}^{(k)} / a_{kk}^{(k)}$, stored below the diagonal in $L$.

**Elimination:** $a_{ij}^{(k+1)} = a_{ij}^{(k)} - m_{i,k} \cdot a_{kj}^{(k)}$

---

**Step 1:** Column 1: $|2|, |4|, |1|$ — largest is $|a_{21}| = 4$. Swap $R_1 \leftrightarrow R_2$:

$$A \sim \begin{bmatrix} 4 & -10 & 2 \\ 2 & 4 & 2 \\ 1 & 2 & 4 \end{bmatrix}$$

Multipliers: $m_{21} = 2/4 = 1/2$, $m_{31} = 1/4$.

Eliminate:
- $R_2 \leftarrow R_2 - \frac{1}{2} R_1$:
  - col 1: $2 - \frac{1}{2}(4) = 2 - 2 = 0$
  - col 2: $4 - \frac{1}{2}(-10) = 4 + 5 = 9$
  - col 3: $2 - \frac{1}{2}(2) = 2 - 1 = 1$
- $R_3 \leftarrow R_3 - \frac{1}{4} R_1$:
  - col 1: $1 - \frac{1}{4}(4) = 1 - 1 = 0$
  - col 2: $2 - \frac{1}{4}(-10) = 2 + \frac{10}{4} = 2 + \frac{5}{2} = \frac{9}{2}$
  - col 3: $4 - \frac{1}{4}(2) = 4 - \frac{1}{2} = \frac{7}{2}$

$$A^{(2)} = \begin{bmatrix} 4 & -10 & 2 \\ 0 & 9 & 1 \\ 0 & 9/2 & 7/2 \end{bmatrix}$$

**Step 2:** Column 2 (rows 2,3): $|9|, |9/2|$ — largest is $|a_{22}^{(2)}| = 9$. No swap.

Multiplier: $m_{32} = (9/2)/9 = 1/2$.

$R_3 \leftarrow R_3 - \frac{1}{2} R_2$:
  - col 2: $\frac{9}{2} - \frac{1}{2}(9) = \frac{9}{2} - \frac{9}{2} = 0$
  - col 3: $\frac{7}{2} - \frac{1}{2}(1) = \frac{7}{2} - \frac{1}{2} = \frac{6}{2} = 3$

**Result:**

$$\boxed{U = \begin{bmatrix} 4 & -10 & 2 \\ 0 & 9 & 1 \\ 0 & 0 & 3 \end{bmatrix}, \quad L = \begin{bmatrix} 1 & 0 & 0 \\ 1/2 & 1 & 0 \\ 1/4 & 1/2 & 1 \end{bmatrix}, \quad P = \begin{bmatrix} 0 & 1 & 0 \\ 1 & 0 & 0 \\ 0 & 0 & 1 \end{bmatrix}}$$

**Check:** $PA = LU$ $\checkmark$

### b) Solve $Ax = b$

**Formula used — solving via LUP:** $Ax = b \iff PAx = Pb \iff LUx = Pb$. Solve in two steps:
1. **Forward substitution** $Ly = Pb$: $y_i = (Pb)_i - \sum_{j=1}^{i-1} l_{ij} y_j$
2. **Backward substitution** $Ux = y$: $x_i = \frac{1}{u_{ii}}\left(y_i - \sum_{j=i+1}^{n} u_{ij} x_j\right)$

---

$P$ swaps rows 1 and 2, so:

$$Pb = P \begin{bmatrix} 6 \\ -10 \\ 9 \end{bmatrix} = \begin{bmatrix} -10 \\ 6 \\ 9 \end{bmatrix}$$

**Forward substitution ($Ly = Pb$):**

Write out the system $Ly = Pb$:

$$\begin{cases} y_1 = -10 \\ \frac{1}{2} y_1 + y_2 = 6 \\ \frac{1}{4} y_1 + \frac{1}{2} y_2 + y_3 = 9 \end{cases}$$

From equation 1: $y_1 = -10$

From equation 2: $y_2 = 6 - \frac{1}{2}(-10) = 6 + 5 = 11$

From equation 3: $y_3 = 9 - \frac{1}{4}(-10) - \frac{1}{2}(11) = 9 + \frac{10}{4} - \frac{11}{2} = 9 + 2.5 - 5.5 = 6$

$$y = \begin{bmatrix} -10 \\ 11 \\ 6 \end{bmatrix}$$

**Backward substitution ($Ux = y$):**

Write out the system $Ux = y$:

$$\begin{cases} 4x_1 - 10x_2 + 2x_3 = -10 \\ 9x_2 + x_3 = 11 \\ 3x_3 = 6 \end{cases}$$

From equation 3: $x_3 = \frac{6}{3} = 2$

From equation 2: $x_2 = \frac{11 - 1 \cdot 2}{9} = \frac{11 - 2}{9} = \frac{9}{9} = 1$

From equation 1: $x_1 = \frac{-10 - (-10)(1) - 2(2)}{4} = \frac{-10 + 10 - 4}{4} = \frac{-4}{4} = -1$

$$\boxed{x = \begin{bmatrix} -1 \\ 1 \\ 2 \end{bmatrix}}$$

**Verification:**

$$Ax = \begin{bmatrix} 2 & 4 & 2 \\ 4 & -10 & 2 \\ 1 & 2 & 4 \end{bmatrix} \begin{bmatrix} -1 \\ 1 \\ 2 \end{bmatrix} = \begin{bmatrix} 2(-1) + 4(1) + 2(2) \\ 4(-1) + (-10)(1) + 2(2) \\ 1(-1) + 2(1) + 4(2) \end{bmatrix} = \begin{bmatrix} -2 + 4 + 4 \\ -4 - 10 + 4 \\ -1 + 2 + 8 \end{bmatrix} = \begin{bmatrix} 6 \\ -10 \\ 9 \end{bmatrix} \checkmark$$
