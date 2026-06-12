# Solution — Numerical Calculus Exam, 6

---

## Problem 1

> (2 points) Consider the iteration $x_{n+1} = 2(1+c)x_n - cx_n^2 - 2$. Find the values of $c \in \mathbb{R}$ for which the method converges to $\alpha = 2$ for a suitable $x_0$. Discuss the convergence of the method, the order of convergence and give a bound for the error.

**Formula used — one-point iteration $x_{n+1} = g(x_n)$:**

- Convergence: $|g'(\alpha)| < 1$.
- If $g'(\alpha) \neq 0$: order $p = 1$, rate $|g'(\alpha)|$.
- If $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: order $p = 2$.
- Error bound: $|x_{n+1} - \alpha| \approx \frac{|g^{(p)}(\alpha)|}{p!}|x_n - \alpha|^p$.

---

$$g(x) = 2(1+c)x - cx^2 - 2$$

**Check fixed point:** $g(2) = 2(1+c)(2) - c(4) - 2 = 4 + 4c - 4c - 2 = 2$ $\checkmark$ (for all $c$)

**Compute $g'$:**

$$g'(x) = 2(1+c) - 2cx$$

$$g'(2) = 2(1+c) - 4c = 2 + 2c - 4c = 2 - 2c$$

**Convergence condition:** $|g'(2)| < 1 \iff |2 - 2c| < 1 \iff -1 < 2 - 2c < 1$

$$-1 < 2 - 2c \implies 2c < 3 \implies c < 3/2$$
$$2 - 2c < 1 \implies 1 < 2c \implies c > 1/2$$

$$\boxed{\frac{1}{2} < c < \frac{3}{2}}$$

**Order of convergence:**

- **General case ($c \neq 1$, $1/2 < c < 3/2$):** $g'(2) = 2 - 2c \neq 0$, so **order $p = 1$**, rate $|2 - 2c|$.

- **Special case $c = 1$:** $g'(2) = 0$. Check $g''(x) = -2c = -2 \neq 0$. So **order $p = 2$**.

**Error bounds:**

For $c \neq 1$: $|x_n - 2| \leq |2-2c|^n \cdot |x_0 - 2|$

For $c = 1$: $|x_{n+1} - 2| \approx \frac{|g''(2)|}{2!}|x_n - 2|^2 = \frac{2}{2}|x_n - 2|^2 = |x_n - 2|^2$

---

## Problem 2

> Let $f \in C^2[0, 1]$.
>
> a) (1.5 points) Find $(Pf)(x)$, the polynomial interpolating the function $f$, given $f'(0)$ and $f(1)$.
>
> b) (1 point) Integrate $\int_{0}^{1}(Pf)(x)\,dx$ to obtain a quadrature formula for $\int_{0}^{1}f(x)\,dx$. Find its degree of precision.
>
> c) (1.5 points) Find the remainder $R(f)$ of the quadrature formula in b), using Peano's theorem.

### a) Find $(Pf)(x)$

**Formula used — Birkhoff interpolation:** Nodes $x_0 = 0$, $x_1 = 1$ with data $f'(0)$ and $f(1)$. So $I_0 = \{1\}$ (derivative), $I_1 = \{0\}$ (function value), $n = 1+1-1 = 1$.

We seek $P(x) = a + bx$ satisfying:

$$P'(0) = f'(0) \implies b = f'(0)$$
$$P(1) = f(1) \implies a + b = f(1) \implies a = f(1) - f'(0)$$

**Using fundamental polynomials** $(Pf)(x) = b_{01}(x)\,f'(0) + b_{10}(x)\,f(1)$:

For $b_{01}(x) = \alpha x + \beta$: $b_{01}'(0) = \alpha = 1$ and $b_{01}(1) = \alpha + \beta = 0 \implies \beta = -1$. So $b_{01}(x) = x - 1$.

For $b_{10}(x) = \alpha x + \beta$: $b_{10}'(0) = \alpha = 0$ and $b_{10}(1) = \beta = 1$. So $b_{10}(x) = 1$.

$$\boxed{(Pf)(x) = (x-1)\,f'(0) + 1 \cdot f(1) = f(1) + (x-1)\,f'(0)}$$

### b) Quadrature formula and degree of precision

$$\int_0^1 (Pf)(x)\,dx = f(1)\int_0^1 1\,dx + f'(0)\int_0^1 (x-1)\,dx$$

**First integral:** $\int_0^1 1\,dx = x\Big|_0^1 = 1 - 0 = 1$

**Second integral:** $\int_0^1 (x-1)\,dx$

Antiderivative: $\int(x-1)\,dx = \frac{x^2}{2} - x$

$$= \left(\frac{x^2}{2} - x\right)\Bigg|_0^1 = \left(\frac{1}{2} - 1\right) - (0 - 0) = -\frac{1}{2}$$

$$\boxed{\int_0^1 f(x)\,dx \approx f(1) - \frac{1}{2}f'(0)}$$

**Degree of precision:** $R(f) = \int_0^1 f(x)\,dx - f(1) + \frac{1}{2}f'(0)$

- $R(e_0)$: $f=1$, $f(1)=1$, $f'(0)=0$. $R = 1 - 1 + 0 = 0$ $\checkmark$
- $R(e_1)$: $f=x$, $f(1)=1$, $f'(0)=1$.

$$\int_0^1 x\,dx = \frac{x^2}{2}\Bigg|_0^1 = \frac{1}{2}$$

$$R = \frac{1}{2} - 1 + \frac{1}{2}(1) = \frac{1}{2} - 1 + \frac{1}{2} = 0 \checkmark$$

- $R(e_2)$: $f=x^2$, $f(1)=1$, $f'(x)=2x$, $f'(0)=0$.

$$\int_0^1 x^2\,dx = \frac{x^3}{3}\Bigg|_0^1 = \frac{1}{3}$$

$$R = \frac{1}{3} - 1 + \frac{1}{2}(0) = \frac{1}{3} - 1 = -\frac{2}{3} \neq 0$$

Degree of precision $d = 1$.

### c) Remainder using Peano's theorem

**Formula used:** $d = 1 \implies R(f) = \int_0^1 K_1(t)\,f''(t)\,dt$ with $K_1(t) = R_x((x-t)_+)$.

**Corollary:** If $K_1$ constant sign, $R(f) = \frac{1}{2!}f''(\xi) R(e_2)$.

---

$$K_1(t) = R_x((x-t)_+) = \int_0^1 (x-t)_+\,dx - (1-t)_+ + \frac{1}{2}\left[\frac{\partial}{\partial x}(x-t)_+\right]_{x=0}$$

For $t \in [0,1]$:
- $(1-t)_+ = 1-t$ (since $t \leq 1$)
- $\frac{\partial}{\partial x}(x-t)_+ = H(x-t)$. At $x=0$: $H(0-t) = H(-t) = 0$ for $t > 0$. (For $t=0$, $H(0)$, we take $= 0$.)

$$\int_0^1(x-t)_+\,dx = \int_t^1 (x-t)\,dx = \frac{(x-t)^2}{2}\Bigg|_t^1 = \frac{(1-t)^2}{2}$$

$$K_1(t) = \frac{(1-t)^2}{2} - (1-t) + 0 = (1-t)\left(\frac{1-t}{2} - 1\right) = (1-t)\cdot\frac{-(1+t)}{2} = -\frac{(1-t)(1+t)}{2}$$

Wait, that's not right. Let me redo this. We have $a = 0$, $b = 1$. The formula is:

$$R(f) = \int_0^1 f(x)\,dx - f(1) + \frac{1}{2}f'(0)$$

So $L = R$ acts as: $Lf = \int_0^1 f(x)\,dx - f(1) + \frac{1}{2}f'(0)$. Note the sign of $f'(0)$ is positive (coefficient $+1/2$).

$$K_1(t) = \int_0^1 (x-t)_+\,dx - (1-t)_+ + \frac{1}{2}\cdot H(0-t) \cdot (-1)$$

Hmm, let me be more careful. The functional is:

$$Lf = \int_0^1 f(x)\,dx - 1\cdot f(1) + \frac{1}{2}\cdot f'(0)$$

Wait, I need to be careful about the signs. The quadrature is $\int \approx f(1) - \frac{1}{2}f'(0)$, so:

$$R(f) = \int_0^1 f(x)\,dx - f(1) + \frac{1}{2}f'(0)$$

Apply to $(x-t)_+$:

$$(x-t)_+\Big|_{x=1} = (1-t)_+$$

$$\frac{\partial}{\partial x}(x-t)_+\Big|_{x=0} = H(-t) = \begin{cases} 1 & t < 0 \\ 0 & t > 0 \end{cases}$$

For $t \in (0,1]$: $H(-t) = 0$. So:

$$K_1(t) = \frac{(1-t)^2}{2} - (1-t) + \frac{1}{2}\cdot 0 = \frac{(1-t)^2}{2} - (1-t)$$

$$= (1-t)\left(\frac{1-t}{2} - 1\right) = (1-t)\cdot\frac{-(1+t)}{2} = -\frac{(1-t)(1+t)}{2}$$

Hmm, $(1+t)$ for $t \in [0,1]$... this gives $-\frac{1-t^2}{2}$ which is $\leq 0$.

Wait, let me recheck: $(1-t)\left(\frac{1-t-2}{2}\right) = (1-t)\frac{-(1+t)}{2}$. Yes.

$K_1(t) = -\frac{(1-t)(1+t)}{2} = -\frac{1-t^2}{2} \leq 0$ for $t \in [0,1]$.

Constant sign, so Corollary applies:

$$R(f) = \frac{1}{2}f''(\xi) \cdot R(e_2) = \frac{1}{2}\left(-\frac{2}{3}\right)f''(\xi) = -\frac{1}{3}f''(\xi)$$

$$\boxed{R(f) = -\frac{1}{3}f''(\xi), \quad \xi \in (0,1)}$$

---

## Problem 3

> (3 points) Use LUP decomposition with partial pivoting to solve the system
>
> $$\begin{bmatrix} 2 & 1 & 1 \\ 2 & 2 & 3 \\ -4 & -1 & 1 \end{bmatrix} \begin{bmatrix} x_1 \\ x_2 \\ x_3 \end{bmatrix} = \begin{bmatrix} 1 \\ 4 \\ 3 \end{bmatrix}$$

**Formula used — LUP factorization:** $PA = LU$, partial pivoting, then solve $Ly = Pb$, $Ux = y$.

---

$$A = \begin{bmatrix} 2 & 1 & 1 \\ 2 & 2 & 3 \\ -4 & -1 & 1 \end{bmatrix}$$

**Step 1:** Column 1: $|2|, |2|, |-4| = 4$ — largest is $|a_{31}| = 4$. Swap $R_1 \leftrightarrow R_3$:

$$A \sim \begin{bmatrix} -4 & -1 & 1 \\ 2 & 2 & 3 \\ 2 & 1 & 1 \end{bmatrix}$$

Multipliers: $m_{21} = 2/(-4) = -1/2$, $m_{31} = 2/(-4) = -1/2$.

$R_2 \leftarrow R_2 - (-\frac{1}{2})R_1 = R_2 + \frac{1}{2}R_1$:
- col 1: $2 + \frac{1}{2}(-4) = 2 - 2 = 0$
- col 2: $2 + \frac{1}{2}(-1) = 2 - \frac{1}{2} = \frac{3}{2}$
- col 3: $3 + \frac{1}{2}(1) = 3 + \frac{1}{2} = \frac{7}{2}$

$R_3 \leftarrow R_3 - (-\frac{1}{2})R_1 = R_3 + \frac{1}{2}R_1$:
- col 1: $2 + \frac{1}{2}(-4) = 0$
- col 2: $1 + \frac{1}{2}(-1) = \frac{1}{2}$
- col 3: $1 + \frac{1}{2}(1) = \frac{3}{2}$

$$A^{(2)} = \begin{bmatrix} -4 & -1 & 1 \\ 0 & 3/2 & 7/2 \\ 0 & 1/2 & 3/2 \end{bmatrix}$$

**Step 2:** Column 2 (rows 2,3): $|3/2|, |1/2|$ — largest is $3/2$. No swap.

$m_{32} = (1/2)/(3/2) = 1/3$.

$R_3 \leftarrow R_3 - \frac{1}{3}R_2$:
- col 2: $\frac{1}{2} - \frac{1}{3}\cdot\frac{3}{2} = \frac{1}{2} - \frac{1}{2} = 0$
- col 3: $\frac{3}{2} - \frac{1}{3}\cdot\frac{7}{2} = \frac{3}{2} - \frac{7}{6} = \frac{9}{6} - \frac{7}{6} = \frac{2}{6} = \frac{1}{3}$

**Result:**

$$\boxed{L = \begin{bmatrix} 1 & 0 & 0 \\ -1/2 & 1 & 0 \\ -1/2 & 1/3 & 1 \end{bmatrix}, \quad U = \begin{bmatrix} -4 & -1 & 1 \\ 0 & 3/2 & 7/2 \\ 0 & 0 & 1/3 \end{bmatrix}, \quad P = \begin{bmatrix} 0 & 0 & 1 \\ 0 & 1 & 0 \\ 1 & 0 & 0 \end{bmatrix}}$$

**Solve:**

$$Pb = P\begin{bmatrix}1\\4\\3\end{bmatrix} = \begin{bmatrix}3\\4\\1\end{bmatrix}$$

**Forward substitution ($Ly = Pb$):**

$$\begin{cases} y_1 = 3 \\ -\frac{1}{2}(3) + y_2 = 4 \implies y_2 = 4 + \frac{3}{2} = \frac{11}{2} \\ -\frac{1}{2}(3) + \frac{1}{3}\cdot\frac{11}{2} + y_3 = 1 \implies -\frac{3}{2} + \frac{11}{6} + y_3 = 1 \implies \frac{-9+11}{6} + y_3 = 1 \implies \frac{2}{6} + y_3 = 1 \implies y_3 = \frac{2}{3} \end{cases}$$

**Backward substitution ($Ux = y$):**

$$\begin{cases} \frac{1}{3}x_3 = \frac{2}{3} \implies x_3 = 2 \\ \frac{3}{2}x_2 + \frac{7}{2}(2) = \frac{11}{2} \implies \frac{3}{2}x_2 = \frac{11}{2} - 7 = -\frac{3}{2} \implies x_2 = -1 \\ -4x_1 - (-1) + 2 = 3 \implies -4x_1 + 3 = 3 \implies x_1 = 0 \end{cases}$$

$$\boxed{x = \begin{bmatrix} 0 \\ -1 \\ 2 \end{bmatrix}}$$

**Verification:**
- $2(0) + 1(-1) + 1(2) = -1 + 2 = 1$ $\checkmark$
- $2(0) + 2(-1) + 3(2) = -2 + 6 = 4$ $\checkmark$
- $-4(0) + (-1)(-1) + 1(2) = 1 + 2 = 3$ $\checkmark$
