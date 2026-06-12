# Solution — Numerical Calculus Exam, 18

---

## Problem 1

> (2 points) Approximate the solution of the system
> $$\begin{bmatrix} 2 & 1 \\ 1 & 2 \end{bmatrix} \begin{bmatrix} x_1 \\ x_2 \end{bmatrix} = \begin{bmatrix} -1 \\ -2 \end{bmatrix}$$
> by the third iteration in the Jacobi method, starting with $x^{(0)} = [0\ 0]^T$.

**Formula used — Jacobi iteration:** $x_i^{(k+1)} = \frac{1}{a_{ii}}\big(b_i - \sum_{j \neq i} a_{ij}x_j^{(k)}\big)$

---

$$x_1^{(k+1)} = \frac{1}{2}(-1 - x_2^{(k)}), \quad x_2^{(k+1)} = \frac{1}{2}(-2 - x_1^{(k)})$$

**Iteration 1** ($x^{(0)} = [0,0]^T$):

$$x_1^{(1)} = \frac{1}{2}(-1-0) = -\frac{1}{2}, \quad x_2^{(1)} = \frac{1}{2}(-2-0) = -1$$

**Iteration 2** ($x^{(1)} = [-1/2, -1]^T$):

$$x_1^{(2)} = \frac{1}{2}(-1-(-1)) = \frac{1}{2}(0) = 0, \quad x_2^{(2)} = \frac{1}{2}(-2-(-1/2)) = \frac{1}{2}(-3/2) = -\frac{3}{4}$$

**Iteration 3** ($x^{(2)} = [0, -3/4]^T$):

$$x_1^{(3)} = \frac{1}{2}(-1-(-3/4)) = \frac{1}{2}(-1/4) = -\frac{1}{8}, \quad x_2^{(3)} = \frac{1}{2}(-2-0) = -1$$

$$\boxed{x^{(3)} = [-1/8,\ -1]^T}$$

*Exact: $x = [0, -1]^T$.*

---

## Problem 2

> (2.5 points) Find the linear polynomial least squares approximation that best fits the data $(0, -2),\ (1, 1),\ (2, 3)$.

**Formula used — discrete least squares normal equations:** $\begin{cases} Na + (\sum x_i)b = \sum y_i \\ (\sum x_i)a + (\sum x_i^2)b = \sum x_iy_i \end{cases}$

---

$N = 3$. $\sum x_i = 3$, $\sum x_i^2 = 5$, $\sum y_i = -2+1+3 = 2$, $\sum x_iy_i = 0+1+6 = 7$.

$$\begin{cases} 3a + 3b = 2 \\ 3a + 5b = 7 \end{cases}$$

$2b = 5 \implies b = 5/2$. $3a = 2-15/2 = -11/2 \implies a = -11/6$.

$$\boxed{\varphi(x) = -\frac{11}{6} + \frac{5}{2}x}$$

---

## Problem 3

> (2.5 points) Determine a 1-point quadrature formula
> $$\int_{-1}^{1} x^2 f(x)\,dx = A_0 f(x_0) + R(f),$$
> having the maximum degree of precision possible. For functions $f \in C^2[-1, 1]$, find an expression for the remainder $R(f)$.

**Formula used — method of undetermined coefficients:** 2 unknowns ($A_0$, $x_0$), max $d = 1$.

---

**$k=0$: $f=1$**

$\int_{-1}^{1} x^2\,dx = \frac{x^3}{3}\Big|_{-1}^{1} = \frac{1}{3}+\frac{1}{3} = \frac{2}{3}$. So $A_0 = \frac{2}{3}$.

**$k=1$: $f=x$**

$\int_{-1}^{1} x^3\,dx = \frac{x^4}{4}\Big|_{-1}^{1} = \frac{1}{4}-\frac{1}{4} = 0$. So $A_0 x_0 = 0 \implies x_0 = 0$.

**Check $k=2$:** $\int_{-1}^{1} x^4\,dx = \frac{x^5}{5}\Big|_{-1}^{1} = \frac{2}{5}$. $A_0 x_0^2 = 0 \neq \frac{2}{5}$. So $d=1$.

$$\boxed{\int_{-1}^{1} x^2 f(x)\,dx = \frac{2}{3}f(0) + R(f)}$$

$R(e_2) = \frac{2}{5}$. Remainder:

$$\boxed{R(f) = \frac{1}{2}f''(\xi)\cdot\frac{2}{5} = \frac{1}{5}f''(\xi), \quad \xi \in (-1,1)}$$

---

## Problem 4

> (2 points) Consider the iteration $x_{n+1} = 2(1+c)x_n - cx_n^2 - 2$. Find the values of $c \in \mathbb{R}$ for which the method converges to $\alpha = 2$, for $x_0$ sufficiently close. Discuss the convergence of the method, the order of convergence and give a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:**

- Convergence: $|g'(\alpha)| < 1$.
- If $g'(\alpha) \neq 0$: order $p = 1$, rate $|g'(\alpha)|$.
- If $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: order $p = 2$.
- Error bound: $|x_{n+1} - \alpha| \approx \frac{|g^{(p)}(\alpha)|}{p!}|x_n - \alpha|^p$.

---

$g(x) = 2(1+c)x - cx^2 - 2$. $g(2) = 4+4c-4c-2 = 2$ $\checkmark$

$g'(x) = 2(1+c)-2cx$. $g'(2) = 2+2c-4c = 2-2c$.

$|2-2c| < 1 \iff \boxed{\frac{1}{2} < c < \frac{3}{2}}$

For $c \neq 1$: order $p=1$, rate $|2-2c|$, error: $|x_n-2| \leq |2-2c|^n|x_0-2|$.

For $c=1$: $g'(2)=0$, $g''=-2\neq 0$, order $p=2$, error: $|x_{n+1}-2| \leq |x_n-2|^2$.
