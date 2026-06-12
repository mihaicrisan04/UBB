# Solution — Numerical Calculus Exam, 2

---

## Problem 1

> (2.5 points) Consider the function $f(x) = 2x^5 - 5x^4 + 10x^2 - 11x + 4$. Show that $\alpha = 1$ is a root of $f$. Does the iteration $x_{n+1} = 2x_n^5 - 5x_n^4 + 10x_n^2 - 10x_n + 4$ converge to $\alpha = 1$ for $x_0$ sufficiently close? Study the convergence of the method, find the order of convergence and give a bound for the error.

**Formula used — convergence of one-point iteration $x_{n+1} = g(x_n)$ (Theorem 3.6):**

If $\alpha$ is a fixed point of $g$ and $|g'(\alpha)| < 1$, then the method converges for $x_0$ sufficiently close to $\alpha$.

**Order of convergence:** If $g'(\alpha) = g''(\alpha) = \cdots = g^{(p-1)}(\alpha) = 0$ and $g^{(p)}(\alpha) \neq 0$, then the method has order $p$, with:

$$\lim_{n \to \infty} \frac{|x_{n+1} - \alpha|}{|x_n - \alpha|^p} = \frac{|g^{(p)}(\alpha)|}{p!}$$

---

**Show $\alpha = 1$ is a root:**

$$f(1) = 2(1)^5 - 5(1)^4 + 10(1)^2 - 11(1) + 4 = 2 - 5 + 10 - 11 + 4 = 0 \checkmark$$

**Fixed-point iteration:** $g(x) = 2x^5 - 5x^4 + 10x^2 - 10x + 4$, so $x_{n+1} = g(x_n)$.

**Check fixed point:** $g(1) = 2 - 5 + 10 - 10 + 4 = 1$ $\checkmark$

Note: $g(x) = f(x) + x$, so $f(\alpha) = 0 \iff g(\alpha) = \alpha$.

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

## Problem 2

> (3 points) Use LUP decomposition with partial pivoting to solve the system
>
> $$\begin{bmatrix} 2 & 4 & 2 \\ 4 & -10 & 2 \\ 1 & 2 & 4 \end{bmatrix} \begin{bmatrix} x_1 \\ x_2 \\ x_3 \end{bmatrix} = \begin{bmatrix} 6 \\ -10 \\ 9 \end{bmatrix}$$

**Formula used — LUP factorization:** Find $L$, $U$, $P$ such that $PA = LU$.

**Partial pivoting:** at step $k$, pivot = largest $|a_{lk}^{(k)}|$ for $l \geq k$. Swap rows if needed.

**Multipliers:** $m_{i,k} = a_{ik}^{(k)} / a_{kk}^{(k)}$. **Elimination:** $a_{ij}^{(k+1)} = a_{ij}^{(k)} - m_{i,k} \cdot a_{kj}^{(k)}$.

**Solving:** $Ax = b \iff LUx = Pb$. Forward sub $Ly = Pb$, then backward sub $Ux = y$.

---

**Step 1:** Column 1: $|2|, |4|, |1|$ — largest is $|a_{21}| = 4$. Swap $R_1 \leftrightarrow R_2$:

$$A \sim \begin{bmatrix} 4 & -10 & 2 \\ 2 & 4 & 2 \\ 1 & 2 & 4 \end{bmatrix}$$

Multipliers: $m_{21} = 2/4 = 1/2$, $m_{31} = 1/4$.

$R_2 \leftarrow R_2 - \frac{1}{2} R_1$:
- col 1: $2 - \frac{1}{2}(4) = 2 - 2 = 0$
- col 2: $4 - \frac{1}{2}(-10) = 4 + 5 = 9$
- col 3: $2 - \frac{1}{2}(2) = 2 - 1 = 1$

$R_3 \leftarrow R_3 - \frac{1}{4} R_1$:
- col 1: $1 - \frac{1}{4}(4) = 0$
- col 2: $2 - \frac{1}{4}(-10) = 2 + \frac{10}{4} = \frac{9}{2}$
- col 3: $4 - \frac{1}{4}(2) = 4 - \frac{1}{2} = \frac{7}{2}$

**Step 2:** Column 2 (rows 2,3): $|9|, |9/2|$ — largest is $9$. No swap.

$m_{32} = (9/2)/9 = 1/2$.

$R_3 \leftarrow R_3 - \frac{1}{2} R_2$:
- col 2: $\frac{9}{2} - \frac{1}{2}(9) = 0$
- col 3: $\frac{7}{2} - \frac{1}{2}(1) = 3$

**Result:**

$$\boxed{L = \begin{bmatrix} 1 & 0 & 0 \\ 1/2 & 1 & 0 \\ 1/4 & 1/2 & 1 \end{bmatrix}, \quad U = \begin{bmatrix} 4 & -10 & 2 \\ 0 & 9 & 1 \\ 0 & 0 & 3 \end{bmatrix}, \quad P = \begin{bmatrix} 0 & 1 & 0 \\ 1 & 0 & 0 \\ 0 & 0 & 1 \end{bmatrix}}$$

**Solve the system:**

$$Pb = \begin{bmatrix} -10 \\ 6 \\ 9 \end{bmatrix}$$

**Forward substitution ($Ly = Pb$):**

$$\begin{cases} y_1 = -10 \\ \frac{1}{2}(-10) + y_2 = 6 \implies y_2 = 6 + 5 = 11 \\ \frac{1}{4}(-10) + \frac{1}{2}(11) + y_3 = 9 \implies -2.5 + 5.5 + y_3 = 9 \implies y_3 = 6 \end{cases}$$

**Backward substitution ($Ux = y$):**

$$\begin{cases} 3x_3 = 6 \implies x_3 = 2 \\ 9x_2 + 1(2) = 11 \implies x_2 = \frac{9}{9} = 1 \\ 4x_1 - 10(1) + 2(2) = -10 \implies 4x_1 = -10 + 10 - 4 = -4 \implies x_1 = -1 \end{cases}$$

$$\boxed{x = \begin{bmatrix} -1 \\ 1 \\ 2 \end{bmatrix}}$$

**Verification:** $Ax = [2(-1)+4(1)+2(2),\ 4(-1)-10(1)+2(2),\ (-1)+2(1)+4(2)]^T = [6,\ -10,\ 9]^T$ $\checkmark$

---

## Problem 3

> Let $f : \mathbb{R} \to \mathbb{R}$ be a twice continuously differentiable function on $[-1, 1]$.
>
> a) (1 point) Find $(Pf)(x)$, the polynomial interpolating the function $f$ at the points $(-1, f(-1))$ and $(1, f(1))$.
>
> b) (1 point) Integrate $\int_{-1}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{-1}^{1}f(x)\,dx$. Find its degree of precision.
>
> c) (1.5 points) Find the remainder $R(f)$ of the quadrature formula in b), using Peano's theorem.

### a) Find $(Pf)(x)$

**Formula used — Lagrange interpolation at $n+1$ simple nodes:**

$$L_n f(x) = \sum_{i=0}^{n} l_i(x) f(x_i), \quad l_i(x) = \prod_{\substack{j=0 \\ j \neq i}}^{n} \frac{x - x_j}{x_i - x_j}$$

Here $n = 1$, $x_0 = -1$, $x_1 = 1$:

$$l_0(x) = \frac{x - 1}{-1 - 1} = \frac{1 - x}{2}, \quad l_1(x) = \frac{x - (-1)}{1 - (-1)} = \frac{x + 1}{2}$$

$$\boxed{(Pf)(x) = \frac{1 - x}{2} f(-1) + \frac{1 + x}{2} f(1)}$$

### b) Quadrature formula and degree of precision

**Formula used:** $\int_a^b f(x)\,dx \approx \sum A_i f(x_i)$ where $A_i = \int_a^b l_i(x)\,dx$.

**Degree of precision:** largest $d$ such that $R(e_k) = 0$ for $k = 0, \ldots, d$, where $R(f) = \int f - \sum A_i f(x_i)$.

---

$$A_0 = \int_{-1}^{1} \frac{1-x}{2}\,dx$$

Antiderivative of $\frac{1-x}{2}$: $\int \frac{1-x}{2}\,dx = \frac{1}{2}\left(x - \frac{x^2}{2}\right)$

$$A_0 = \frac{1}{2}\left(x - \frac{x^2}{2}\right)\Bigg|_{-1}^{1} = \frac{1}{2}\left[\left(1 - \frac{1}{2}\right) - \left(-1 - \frac{1}{2}\right)\right] = \frac{1}{2}\left[\frac{1}{2} + \frac{3}{2}\right] = \frac{1}{2} \cdot 2 = 1$$

$$A_1 = \int_{-1}^{1} \frac{1+x}{2}\,dx = \frac{1}{2}\left(x + \frac{x^2}{2}\right)\Bigg|_{-1}^{1} = \frac{1}{2}\left[\frac{3}{2} + \frac{1}{2}\right] = 1$$

$$\boxed{\int_{-1}^{1} f(x)\,dx \approx f(-1) + f(1)}$$

**Degree of precision:**

- $R(e_0)$: $\int_{-1}^{1} 1\,dx = x\Big|_{-1}^{1} = 1-(-1) = 2$. Quadrature: $1 + 1 = 2$. $R = 0$ $\checkmark$
- $R(e_1)$: $\int_{-1}^{1} x\,dx = \frac{x^2}{2}\Big|_{-1}^{1} = \frac{1}{2} - \frac{1}{2} = 0$. Quadrature: $(-1) + 1 = 0$. $R = 0$ $\checkmark$
- $R(e_2)$: $\int_{-1}^{1} x^2\,dx = \frac{x^3}{3}\Big|_{-1}^{1} = \frac{1}{3} - \frac{-1}{3} = \frac{2}{3}$. Quadrature: $1 + 1 = 2$. $R = \frac{2}{3} - 2 = -\frac{4}{3} \neq 0$

Degree of precision $d = 1$.

### c) Remainder using Peano's theorem

**Formula used — Peano's theorem:** If $\ker L = \mathbb{P}_n$ (degree of precision $d = n$), then $Lf = \int_a^b K_n(t) f^{(n+1)}(t)\,dt$ with $K_n(t) = \frac{1}{n!}L_x((x-t)_+^n)$.

**Corollary:** If $K_n$ has constant sign, then $Lf = \frac{1}{(n+1)!}f^{(n+1)}(\xi) \cdot L(e_{n+1})$.

---

Here $d = 1$, so $n = 1$. $R(f) = \int_{-1}^{1} f(x)\,dx - f(-1) - f(1)$.

$$K_1(t) = R_x((x-t)_+) = \int_{-1}^{1}(x-t)_+\,dx - (-1-t)_+ - (1-t)_+$$

For $t \in [-1,1]$: $(-1-t)_+ = 0$, $(1-t)_+ = 1-t$.

$$\int_{-1}^{1}(x-t)_+\,dx = \int_t^1 (x-t)\,dx$$

Antiderivative of $(x-t)$: $\frac{(x-t)^2}{2}$

$$= \frac{(x-t)^2}{2}\Bigg|_t^1 = \frac{(1-t)^2}{2} - \frac{0}{2} = \frac{(1-t)^2}{2}$$

$$K_1(t) = \frac{(1-t)^2}{2} - (1-t) = (1-t)\left(\frac{1-t}{2} - 1\right) = -\frac{(1-t)(1+t)}{2} = -\frac{1-t^2}{2} \leq 0$$

Constant sign $\implies$ Corollary applies:

$$R(f) = \frac{1}{2!}f''(\xi) \cdot R(e_2) = \frac{1}{2} \cdot f''(\xi) \cdot \left(-\frac{4}{3}\right)$$

$$\boxed{R(f) = -\frac{2}{3}f''(\xi), \quad \xi \in (-1,1)}$$
