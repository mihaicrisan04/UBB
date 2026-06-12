# Solution — Numerical Calculus Exam, 15

---

## Problem 1

> (2.5) Let $f \in C^2[-1, 1]$ twice differentiable. Derive a 1-point quadrature formula
> $$\int_{-1}^{1} x^2 f(x)\,dx = A_0 f(x_0) + R(f)$$
> with max degree of precision. Express remainder $R(f)$ in terms of $f$.

**Formula used — method of undetermined coefficients:** We have 2 unknowns ($A_0$, $x_0$), so we can satisfy at most 2 equations $R(e_k) = 0$, giving max degree of precision $d = 1$.

Set $R(e_k) = 0$ for $k = 0, 1$:

**$k=0$: $f(x) = 1$**

$$\int_{-1}^{1} x^2 \cdot 1\,dx = A_0 \cdot 1$$

$$\int_{-1}^{1} x^2\,dx = \frac{x^3}{3}\Bigg|_{-1}^{1} = \frac{1}{3} - \frac{-1}{3} = \frac{1}{3} + \frac{1}{3} = \frac{2}{3}$$

$$A_0 = \frac{2}{3}$$

**$k=1$: $f(x) = x$**

$$\int_{-1}^{1} x^2 \cdot x\,dx = A_0 \cdot x_0$$

$$\int_{-1}^{1} x^3\,dx = \frac{x^4}{4}\Bigg|_{-1}^{1} = \frac{1}{4} - \frac{1}{4} = 0$$

$$\frac{2}{3} x_0 = 0 \implies x_0 = 0$$

**Check $k=2$:** $f(x) = x^2$

$$\int_{-1}^{1} x^2 \cdot x^2\,dx = \int_{-1}^{1} x^4\,dx = \frac{x^5}{5}\Bigg|_{-1}^{1} = \frac{1}{5} + \frac{1}{5} = \frac{2}{5}$$

$$A_0 x_0^2 = \frac{2}{3}(0)^2 = 0 \neq \frac{2}{5}$$

So $d = 1$.

$$\boxed{\int_{-1}^{1} x^2 f(x)\,dx = \frac{2}{3}f(0) + R(f)}$$

**Remainder:** Since $d = 1$, $R(e_2) = \frac{2}{5} - 0 = \frac{2}{5}$.

By Peano's theorem (if kernel has constant sign):

$$R(f) = \frac{1}{2!}f''(\xi) \cdot R(e_2) = \frac{1}{2}\cdot\frac{2}{5}\cdot f''(\xi) = \frac{1}{5}f''(\xi), \quad \xi \in (-1,1)$$

$$\boxed{R(f) = \frac{1}{5}f''(\xi), \quad \xi \in (-1,1)}$$

---

## Problem 2

> $\begin{cases} 2x_1 + x_2 = -1 \\ x_1 + 2x_2 = -2 \end{cases}$
>
> Approximate solution by third Jacobi method iteration, starting with $x^{(0)} = [0,\ 0]^T$.

**Formula used — Jacobi iteration:** $A = D - L - U$, $T_J = D^{-1}(L+U)$, $c_J = D^{-1}b$.

Component-wise: $x_i^{(k+1)} = \frac{1}{a_{ii}}\left(b_i - \sum_{j \neq i} a_{ij} x_j^{(k)}\right)$

---

$$x_1^{(k+1)} = \frac{1}{2}\left(-1 - x_2^{(k)}\right), \quad x_2^{(k+1)} = \frac{1}{2}\left(-2 - x_1^{(k)}\right)$$

**Iteration 1** ($k=0$, $x^{(0)} = [0, 0]^T$):

$$x_1^{(1)} = \frac{1}{2}(-1 - 0) = -\frac{1}{2} = -0.5$$
$$x_2^{(1)} = \frac{1}{2}(-2 - 0) = -1$$

**Iteration 2** ($k=1$, $x^{(1)} = [-0.5, -1]^T$):

$$x_1^{(2)} = \frac{1}{2}(-1 - (-1)) = \frac{1}{2}(0) = 0$$
$$x_2^{(2)} = \frac{1}{2}(-2 - (-0.5)) = \frac{1}{2}(-1.5) = -0.75$$

**Iteration 3** ($k=2$, $x^{(2)} = [0, -0.75]^T$):

$$x_1^{(3)} = \frac{1}{2}(-1 - (-0.75)) = \frac{1}{2}(-0.25) = -0.125$$
$$x_2^{(3)} = \frac{1}{2}(-2 - 0) = -1$$

$$\boxed{x^{(3)} = \begin{bmatrix} -0.125 \\ -1 \end{bmatrix} = \begin{bmatrix} -1/8 \\ -1 \end{bmatrix}}$$

*Exact solution: $x = [0, -1]^T$ (solve: $2(0)+(-1)=-1$, $0+2(-1)=-2$).*

---

## Problem 3

> | $x$ | 0 | 1 | 2 |
> |---|---|---|---|
> | $y$ | -2 | 1 | 3 |
>
> Best linear polynomial approximation for this.

**Formula used — discrete least squares:** Fit $\varphi(x) = a + bx$ to minimize $E^2 = \sum_{i} (y_i - a - bx_i)^2$.

**Normal equations:** $\begin{cases} Na + (\sum x_i)b = \sum y_i \\ (\sum x_i)a + (\sum x_i^2)b = \sum x_i y_i \end{cases}$ where $N$ = number of data points.

---

$N = 3$, data: $(0,-2), (1,1), (2,3)$.

$$\sum x_i = 0 + 1 + 2 = 3$$
$$\sum x_i^2 = 0 + 1 + 4 = 5$$
$$\sum y_i = -2 + 1 + 3 = 2$$
$$\sum x_i y_i = 0(-2) + 1(1) + 2(3) = 0 + 1 + 6 = 7$$

Normal equations:

$$\begin{cases} 3a + 3b = 2 \\ 3a + 5b = 7 \end{cases}$$

Subtract first from second: $2b = 5 \implies b = 5/2$.

From first: $3a = 2 - 3(5/2) = 2 - 15/2 = -11/2 \implies a = -11/6$.

$$\boxed{\varphi(x) = -\frac{11}{6} + \frac{5}{2}x}$$

---

## Problem 4

> Values of $c \in \mathbb{R}$ for which the iterative method $x_{n+1} = 2(1+c)x_n - cx_n^2 - 2$ converges to $\alpha = 2$, for $x_0$ sufficiently close. What is the order of convergence? Find a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:**

- Convergence: $|g'(\alpha)| < 1$.
- If $g'(\alpha) \neq 0$: order $p = 1$, rate $|g'(\alpha)|$.
- If $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: order $p = 2$.
- Error bound: $|x_{n+1} - \alpha| \approx \frac{|g^{(p)}(\alpha)|}{p!}|x_n - \alpha|^p$.

---

$g(x) = 2(1+c)x - cx^2 - 2$. $g(2) = 4+4c-4c-2 = 2$ $\checkmark$

$g'(x) = 2(1+c) - 2cx$. $g'(2) = 2+2c-4c = 2-2c$.

$|g'(2)| < 1 \iff |2-2c| < 1 \iff \boxed{\frac{1}{2} < c < \frac{3}{2}}$

For $c \neq 1$: order $p = 1$, rate $|2-2c|$, error: $|x_n - 2| \leq |2-2c|^n|x_0-2|$.

For $c = 1$: $g'(2)=0$, $g''(x)=-2c=-2 \neq 0$, order $p = 2$, error: $|x_{n+1}-2| \leq |x_n-2|^2$.
