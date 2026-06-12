# Solution — Numerical Calculus Exam, 5

---

## Problem 1

> (3 points) Solve the system
> $$\begin{cases} -x_1 + 3x_2 + 2x_3 = -1 \\ 2x_1 + 4x_2 = 8 \\ x_1 + 2x_2 - 3x_3 = 7 \end{cases}$$
> using LUP decomposition with partial pivoting.

**Formula used — LUP factorization:** Find $L$, $U$, $P$ such that $PA = LU$.

**Partial pivoting:** at step $k$, pivot = largest $|a_{lk}^{(k)}|$ for $l \geq k$.

**Multipliers:** $m_{i,k} = a_{ik}^{(k)} / a_{kk}^{(k)}$. **Elimination:** $a_{ij}^{(k+1)} = a_{ij}^{(k)} - m_{i,k} \cdot a_{kj}^{(k)}$.

**Solving:** $Ax = b \iff LUx = Pb$. Forward sub $Ly = Pb$, backward sub $Ux = y$.

---

$$A = \begin{bmatrix} -1 & 3 & 2 \\ 2 & 4 & 0 \\ 1 & 2 & -3 \end{bmatrix}, \quad b = \begin{bmatrix} -1 \\ 8 \\ 7 \end{bmatrix}$$

**Step 1:** Column 1: $|-1| = 1,\ |2| = 2,\ |1| = 1$ — largest is $|a_{21}| = 2$. Swap $R_1 \leftrightarrow R_2$:

$$A \sim \begin{bmatrix} 2 & 4 & 0 \\ -1 & 3 & 2 \\ 1 & 2 & -3 \end{bmatrix}$$

Multipliers: $m_{21} = -1/2$, $m_{31} = 1/2$.

$R_2 \leftarrow R_2 - (-\frac{1}{2}) R_1 = R_2 + \frac{1}{2}R_1$:
- col 1: $-1 + \frac{1}{2}(2) = -1 + 1 = 0$
- col 2: $3 + \frac{1}{2}(4) = 3 + 2 = 5$
- col 3: $2 + \frac{1}{2}(0) = 2 + 0 = 2$

$R_3 \leftarrow R_3 - \frac{1}{2} R_1$:
- col 1: $1 - \frac{1}{2}(2) = 1 - 1 = 0$
- col 2: $2 - \frac{1}{2}(4) = 2 - 2 = 0$
- col 3: $-3 - \frac{1}{2}(0) = -3 - 0 = -3$

$$A^{(2)} = \begin{bmatrix} 2 & 4 & 0 \\ 0 & 5 & 2 \\ 0 & 0 & -3 \end{bmatrix}$$

**Step 2:** Column 2 (rows 2,3): $|5|, |0|$ — largest is $5$. No swap.

$m_{32} = 0/5 = 0$. No elimination needed (already zero).

**Result:**

$$\boxed{L = \begin{bmatrix} 1 & 0 & 0 \\ -1/2 & 1 & 0 \\ 1/2 & 0 & 1 \end{bmatrix}, \quad U = \begin{bmatrix} 2 & 4 & 0 \\ 0 & 5 & 2 \\ 0 & 0 & -3 \end{bmatrix}, \quad P = \begin{bmatrix} 0 & 1 & 0 \\ 1 & 0 & 0 \\ 0 & 0 & 1 \end{bmatrix}}$$

**Solve:**

$$Pb = \begin{bmatrix} 8 \\ -1 \\ 7 \end{bmatrix}$$

**Forward substitution ($Ly = Pb$):**

$$\begin{cases} y_1 = 8 \\ -\frac{1}{2}(8) + y_2 = -1 \implies -4 + y_2 = -1 \implies y_2 = 3 \\ \frac{1}{2}(8) + 0(3) + y_3 = 7 \implies 4 + y_3 = 7 \implies y_3 = 3 \end{cases}$$

**Backward substitution ($Ux = y$):**

$$\begin{cases} -3x_3 = 3 \implies x_3 = -1 \\ 5x_2 + 2(-1) = 3 \implies 5x_2 = 5 \implies x_2 = 1 \\ 2x_1 + 4(1) + 0(-1) = 8 \implies 2x_1 = 4 \implies x_1 = 2 \end{cases}$$

$$\boxed{x = \begin{bmatrix} 2 \\ 1 \\ -1 \end{bmatrix}}$$

**Verification:**
- $-(2) + 3(1) + 2(-1) = -2 + 3 - 2 = -1$ $\checkmark$
- $2(2) + 4(1) + 0(-1) = 4 + 4 = 8$ $\checkmark$
- $2 + 2(1) - 3(-1) = 2 + 2 + 3 = 7$ $\checkmark$

---

## Problem 2

> (2 points) Find the values of $c \in \mathbb{R}$ for which the iterative method $x_{n+1} = 2 - (1+c)x_n + cx_n^2$ converges to $\alpha = 1$ for a suitable $x_0$. Discuss the convergence of the method, the order of convergence and give a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:**

- Convergence condition: $|g'(\alpha)| < 1$ (Theorem 3.6).
- If $g'(\alpha) \neq 0$: order $p = 1$ (linear), rate $\lambda = |g'(\alpha)|$.
- If $g'(\alpha) = 0$ and $g''(\alpha) \neq 0$: order $p = 2$ (quadratic).
- Error bound (linear): $|x_n - \alpha| \leq |g'(\alpha)|^n |x_0 - \alpha|$.

---

$$g(x) = 2 - (1+c)x + cx^2$$

**Check fixed point:** $g(1) = 2 - (1+c) + c = 2 - 1 - c + c = 1$ $\checkmark$ (for all $c$)

**Compute $g'$:**

$$g'(x) = -(1+c) + 2cx$$

$$g'(1) = -(1+c) + 2c = -1 - c + 2c = c - 1$$

**Convergence condition:** $|g'(1)| < 1 \iff |c - 1| < 1 \iff -1 < c - 1 < 1$

$$\boxed{0 < c < 2}$$

**Order of convergence:**

- **General case ($c \neq 1$, $0 < c < 2$):** $g'(1) = c - 1 \neq 0$, so **order $p = 1$** (linear), rate $\lambda = |c-1|$.

- **Special case $c = 1$:** $g'(1) = 0$. Check $g''(x) = 2c = 2 \neq 0$. So **order $p = 2$** (quadratic).

**Error bounds:**

For $c \neq 1$: $|x_n - 1| \leq |c-1|^n \cdot |x_0 - 1|$

For $c = 1$: $|x_{n+1} - 1| \approx \frac{|g''(1)|}{2!}|x_n - 1|^2 = \frac{2}{2}|x_n - 1|^2 = |x_n - 1|^2$

---

## Problem 3

> Let $f \in C^2[-1, 1]$.
>
> a) (1.5 points) Find $(Pf)(x)$, the polynomial interpolating the function $f$, given $f(-1)$ and $f'(1)$.
>
> b) (1 point) Integrate $\int_{-1}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{-1}^{1}f(x)\,dx$. Find its degree of precision.
>
> c) (1.5 points) Find the remainder $R(f)$ of the quadrature formula in b), using Peano's theorem.

### a) Find $(Pf)(x)$

**Formula used — Birkhoff interpolation:** We have two nodes $x_0 = -1$, $x_1 = 1$ with data $f(x_0) = f(-1)$ and $f'(x_1) = f'(1)$. This is $I_0 = \{0\}$, $I_1 = \{1\}$, so $n = |I_0| + |I_1| - 1 = 1$.

We seek $P(x) = a + bx$ (degree $\leq 1$) satisfying:

$$P(-1) = f(-1) \implies a - b = f(-1)$$
$$P'(1) = f'(1) \implies b = f'(1)$$

So $b = f'(1)$ and $a = f(-1) + f'(1)$.

**Alternative — using fundamental polynomials** $b_{00}(x)$ and $b_{11}(x)$:

$(Pf)(x) = b_{00}(x) f(-1) + b_{11}(x) f'(1)$ where each $b_{kj}$ is degree $\leq 1$: $b_{kj}(x) = \alpha x + \beta$.

For $b_{00}$: $b_{00}(-1) = 1$ and $b_{00}'(1) = 0$. Since $b_{00}'(x) = \alpha$, we need $\alpha = 0$, so $b_{00}(x) = \beta$. From $b_{00}(-1) = \beta = 1$: $b_{00}(x) = 1$.

For $b_{11}$: $b_{11}(-1) = 0$ and $b_{11}'(1) = 1$. Since $b_{11}'(x) = \alpha = 1$ and $b_{11}(-1) = -\alpha + \beta = -1 + \beta = 0 \implies \beta = 1$: $b_{11}(x) = x + 1$.

$$\boxed{(Pf)(x) = 1 \cdot f(-1) + (x + 1) \cdot f'(1) = f(-1) + (1 + x)\,f'(1)}$$

### b) Quadrature formula and degree of precision

**Formula used:** Integrate $(Pf)(x)$ to get a quadrature formula, then test $R(e_k) = 0$ for $e_k(x) = x^k$.

---

$$\int_{-1}^{1} (Pf)(x)\,dx = f(-1)\int_{-1}^{1} 1\,dx + f'(1)\int_{-1}^{1}(1+x)\,dx$$

**First integral:** $\int_{-1}^{1} 1\,dx = x\Big|_{-1}^{1} = 1 - (-1) = 2$

**Second integral:** $\int_{-1}^{1} (1+x)\,dx$

Antiderivative: $\int(1+x)\,dx = x + \frac{x^2}{2}$

$$= \left(x + \frac{x^2}{2}\right)\Bigg|_{-1}^{1} = \left(1 + \frac{1}{2}\right) - \left(-1 + \frac{1}{2}\right) = \frac{3}{2} - \left(-\frac{1}{2}\right) = \frac{3}{2} + \frac{1}{2} = 2$$

$$\boxed{\int_{-1}^{1} f(x)\,dx \approx 2\,f(-1) + 2\,f'(1)}$$

**Degree of precision:** $R(f) = \int_{-1}^{1} f(x)\,dx - 2f(-1) - 2f'(1)$

- $R(e_0)$: $f(x) = 1$, $f(-1) = 1$, $f'(1) = 0$. $R = 2 - 2(1) - 2(0) = 0$ $\checkmark$
- $R(e_1)$: $f(x) = x$, $f(-1) = -1$, $f'(1) = 1$. $R = 0 - 2(-1) - 2(1) = 0 + 2 - 2 = 0$ $\checkmark$
- $R(e_2)$: $f(x) = x^2$, $f(-1) = 1$, $f'(x) = 2x$, $f'(1) = 2$.

$$\int_{-1}^{1} x^2\,dx = \frac{x^3}{3}\Bigg|_{-1}^{1} = \frac{1}{3} + \frac{1}{3} = \frac{2}{3}$$

$$R = \frac{2}{3} - 2(1) - 2(2) = \frac{2}{3} - 2 - 4 = \frac{2}{3} - 6 = -\frac{16}{3} \neq 0$$

Degree of precision $d = 1$.

### c) Remainder using Peano's theorem

**Formula used — Peano's theorem:** $d = 1 \implies R(f) = \int_{-1}^{1} K_1(t)\,f''(t)\,dt$ with $K_1(t) = R_x((x-t)_+)$.

**Corollary:** If $K_1$ has constant sign, $R(f) = \frac{1}{2!}f''(\xi) \cdot R(e_2)$.

---

$$R_x((x-t)_+) = \int_{-1}^{1}(x-t)_+\,dx - 2(-1-t)_+ - 2\left[\frac{\partial}{\partial x}(x-t)_+\right]_{x=1}$$

For $t \in [-1,1]$:
- $(-1-t)_+ = 0$
- $\frac{\partial}{\partial x}(x-t)_+ = H(x-t)$ (Heaviside), so at $x=1$: $H(1-t) = 1$ for $t < 1$.

$$\int_{-1}^{1}(x-t)_+\,dx = \int_t^1(x-t)\,dx = \frac{(x-t)^2}{2}\Bigg|_t^1 = \frac{(1-t)^2}{2}$$

$$K_1(t) = \frac{(1-t)^2}{2} - 0 - 2(1) = \frac{(1-t)^2}{2} - 2$$

Check sign: $(1-t)^2 \leq 4$ for $t \in [-1,1]$ (max at $t=-1$), so $K_1(t) \leq \frac{4}{2} - 2 = 0$.

At $t = -1$: $K_1(-1) = \frac{4}{2} - 2 = 0$. At $t = 1$: $K_1(1) = 0 - 2 = -2$. At $t = 0$: $K_1(0) = \frac{1}{2} - 2 = -\frac{3}{2}$.

So $K_1(t) \leq 0$ for all $t \in [-1,1]$ (constant sign). Corollary applies:

$$R(f) = \frac{1}{2}f''(\xi) \cdot R(e_2) = \frac{1}{2} \cdot f''(\xi) \cdot \left(-\frac{16}{3}\right) = -\frac{8}{3}f''(\xi)$$

$$\boxed{R(f) = -\frac{8}{3}f''(\xi), \quad \xi \in (-1,1)}$$
