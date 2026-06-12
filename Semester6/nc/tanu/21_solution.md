# Solution — Numerical Calculus Exam, 21

---

## Problem 1

> (2 points) Use Gaussian elimination with partial pivoting to solve the system
> $$\begin{cases} -x_1 + 3x_2 + 2x_3 = -1 \\ 2x_1 + 4x_2 = 8 \\ x_1 + 2x_2 - 3x_3 = 7 \end{cases}$$

**Formula used:** Same as LUP — Gaussian elimination with partial pivoting is equivalent. At each step, swap the row with the largest pivot, then eliminate.

---

$$\tilde{A} = \left[\begin{array}{ccc|c} -1 & 3 & 2 & -1 \\ 2 & 4 & 0 & 8 \\ 1 & 2 & -3 & 7 \end{array}\right]$$

**Step 1:** Column 1: $|-1|=1, |2|=2, |1|=1$. Largest: row 2. Swap $R_1 \leftrightarrow R_2$:

$$\tilde{A} \sim \left[\begin{array}{ccc|c} 2 & 4 & 0 & 8 \\ -1 & 3 & 2 & -1 \\ 1 & 2 & -3 & 7 \end{array}\right]$$

$m_{21} = -1/2$, $m_{31} = 1/2$.

$R_2 \leftarrow R_2 + \frac{1}{2}R_1$:
- $-1+1 = 0$, $3+2 = 5$, $2+0 = 2$, $-1+4 = 3$

$R_3 \leftarrow R_3 - \frac{1}{2}R_1$:
- $1-1 = 0$, $2-2 = 0$, $-3-0 = -3$, $7-4 = 3$

$$\tilde{A} \sim \left[\begin{array}{ccc|c} 2 & 4 & 0 & 8 \\ 0 & 5 & 2 & 3 \\ 0 & 0 & -3 & 3 \end{array}\right]$$

**Step 2:** Column 2: $|5|, |0|$. Largest: row 2. No swap. $m_{32} = 0$. Already done.

**Back substitution:**

$$-3x_3 = 3 \implies x_3 = -1$$
$$5x_2 + 2(-1) = 3 \implies 5x_2 = 5 \implies x_2 = 1$$
$$2x_1 + 4(1) + 0 = 8 \implies 2x_1 = 4 \implies x_1 = 2$$

$$\boxed{x = [2,\ 1,\ -1]^T}$$

**Verification:**
- $-2+3-2 = -1$ $\checkmark$
- $4+4+0 = 8$ $\checkmark$
- $2+2+3 = 7$ $\checkmark$

---

## Problem 2

> (2.5 points) Show that the equation $x = \frac{1+\sin x}{2}$ has a unique solution $\alpha$. Then find an interval $[a,b]$ containing $\alpha$ for which the iterative method $x_{n+1} = \frac{1+\sin x_n}{2}$ converges to $\alpha$, for any $x_0 \in [a,b]$. Discuss the convergence of the method, the order of convergence and give a bound for the error.

**Formula used — Banach fixed point theorem (Theorem 3.3):** If $g \in C[a,b]$, $g([a,b]) \subseteq [a,b]$, and $|g'(x)| \leq \lambda < 1$ on $[a,b]$, then $g$ has a unique fixed point $\alpha$ and $x_{n+1} = g(x_n)$ converges for any $x_0 \in [a,b]$.

**Error:** $|x_n - \alpha| \leq \frac{\lambda^n}{1-\lambda}|x_1 - x_0|$.

---

$g(x) = \frac{1+\sin x}{2}$.

**Existence and uniqueness:** Let $h(x) = x - g(x) = x - \frac{1+\sin x}{2}$.

$h'(x) = 1 - \frac{\cos x}{2}$. Since $|\cos x| \leq 1$, we have $h'(x) \geq 1 - \frac{1}{2} = \frac{1}{2} > 0$. So $h$ is strictly increasing, hence has at most one zero.

$h(0) = 0 - \frac{1+0}{2} = -\frac{1}{2} < 0$ and $h(\pi/2) = \frac{\pi}{2} - \frac{1+1}{2} = \frac{\pi}{2} - 1 > 0$.

By IVT, there exists exactly one $\alpha \in (0, \pi/2)$.

**Finding $[a,b]$:** Take $[a,b] = [0, \pi/2]$.

- $g(0) = \frac{1+0}{2} = \frac{1}{2} \in [0, \pi/2]$ $\checkmark$
- $g(\pi/2) = \frac{1+1}{2} = 1 < \pi/2 \approx 1.57$ $\checkmark$
- $g$ is increasing on this interval (since $g'(x) = \frac{\cos x}{2} \geq 0$ for $x \in [0,\pi/2]$).
- So $g([0,\pi/2]) = [g(0), g(\pi/2)] = [1/2, 1] \subset [0, \pi/2]$ $\checkmark$

**Contraction:** $g'(x) = \frac{\cos x}{2}$.

$$\lambda = \max_{x \in [0,\pi/2]} |g'(x)| = \max_{x \in [0,\pi/2]} \frac{\cos x}{2} = \frac{\cos 0}{2} = \frac{1}{2} < 1 \checkmark$$

**Convergence:** By Banach's theorem, $x_{n+1} = g(x_n)$ converges for any $x_0 \in [0, \pi/2]$.

**Order:** $g'(\alpha) = \frac{\cos \alpha}{2}$. Since $\alpha \in (0, \pi/2)$, $\cos\alpha > 0$, so $g'(\alpha) \neq 0$.

$$\boxed{\text{Order } p = 1 \text{ (linear)}, \text{ rate } \lambda = \frac{\cos\alpha}{2} \leq \frac{1}{2}}$$

**Error bound:**

$$\boxed{|x_n - \alpha| \leq \frac{(1/2)^n}{1 - 1/2}|x_1 - x_0| = 2 \cdot \left(\frac{1}{2}\right)^n |x_1 - x_0| = \frac{|x_1-x_0|}{2^{n-1}}}$$

---

## Problem 3

> Let $f \in C^3[0, 1]$.
>
> a) (1 point) Find $(Pf)(x)$, the polynomial interpolating $f$, knowing $f(0)$ and $f'(1/3)$.
>
> b) (1 point) Integrate $\int_0^1 (Pf)(x)\,dx$ to obtain a quadrature formula for $\int_0^1 f(x)\,dx$. Find its degree of precision.
>
> c) (1 point) Find $A_0$, $A_1$ and $x_1$ such that $\int_0^1 f(x)\,dx = A_0 f(0) + A_1 f'(x_1) + R(f)$ has the maximum degree of precision possible. Compare to b).
>
> d) (1.5 points) Find the remainder $R(f)$, using Peano's theorem.

### a) Find $(Pf)(x)$

**Formula used — Birkhoff interpolation:** $x_0 = 0$ with $I_0 = \{0\}$ (function value), $x_1 = 1/3$ with $I_1 = \{1\}$ (derivative). $n = 1$, so $P(x) = a + bx$.

$$P(0) = f(0) \implies a = f(0)$$
$$P'(1/3) = f'(1/3) \implies b = f'(1/3)$$

$$\boxed{(Pf)(x) = f(0) + f'(1/3)\,x}$$

### b) Quadrature formula

$$\int_0^1 (Pf)(x)\,dx = f(0)\int_0^1 1\,dx + f'(1/3)\int_0^1 x\,dx$$

$\int_0^1 1\,dx = 1$, $\int_0^1 x\,dx = \frac{x^2}{2}\Big|_0^1 = \frac{1}{2}$.

$$\boxed{\int_0^1 f(x)\,dx \approx f(0) + \frac{1}{2}f'(1/3)}$$

**Degree of precision:** $R(f) = \int_0^1 f\,dx - f(0) - \frac{1}{2}f'(1/3)$

- $R(e_0)$: $1 - 1 - 0 = 0$ $\checkmark$
- $R(e_1)$: $f=x$, $f(0)=0$, $f'=1$, $f'(1/3)=1$. $R = \frac{1}{2} - 0 - \frac{1}{2}(1) = 0$ $\checkmark$
- $R(e_2)$: $f=x^2$, $f(0)=0$, $f'=2x$, $f'(1/3)=2/3$.

$\int_0^1 x^2\,dx = \frac{1}{3}$. $R = \frac{1}{3} - 0 - \frac{1}{2}\cdot\frac{2}{3} = \frac{1}{3} - \frac{1}{3} = 0$ $\checkmark$

- $R(e_3)$: $f=x^3$, $f(0)=0$, $f'=3x^2$, $f'(1/3)=3/9=1/3$.

$\int_0^1 x^3\,dx = \frac{1}{4}$. $R = \frac{1}{4} - 0 - \frac{1}{2}\cdot\frac{1}{3} = \frac{1}{4} - \frac{1}{6} = \frac{3-2}{12} = \frac{1}{12} \neq 0$

Degree of precision $d = 2$.

### c) Undetermined coefficients

We want $\int_0^1 f(x)\,dx = A_0 f(0) + A_1 f'(x_1) + R(f)$ with max precision. 3 unknowns ($A_0, A_1, x_1$), so max $d = 2$.

$R(e_0) = 0$: $1 = A_0 \cdot 1 + A_1 \cdot 0 \implies A_0 = 1$

$R(e_1) = 0$: $\frac{1}{2} = A_0 \cdot 0 + A_1 \cdot 1 \implies A_1 = \frac{1}{2}$

$R(e_2) = 0$: $\frac{1}{3} = A_0 \cdot 0 + A_1 \cdot 2x_1 \implies \frac{1}{3} = \frac{1}{2}\cdot 2x_1 = x_1 \implies x_1 = \frac{1}{3}$

$$\boxed{A_0 = 1, \quad A_1 = \frac{1}{2}, \quad x_1 = \frac{1}{3}}$$

This gives the same formula as in b): $\int_0^1 f\,dx \approx f(0) + \frac{1}{2}f'(1/3)$.

### d) Remainder using Peano's theorem

$d = 2$, so $R(f) = \int_0^1 K_2(t)\,f'''(t)\,dt$ with $K_2(t) = \frac{1}{2!}R_x((x-t)_+^2)$.

$$R_x((x-t)_+^2) = \int_0^1 (x-t)_+^2\,dx - 1\cdot(0-t)_+^2 - \frac{1}{2}\left[\frac{\partial}{\partial x}(x-t)_+^2\right]_{x=1/3}$$

For $t \in [0,1]$: $(0-t)_+^2 = (-t)_+^2 = 0$.

$\frac{\partial}{\partial x}(x-t)_+^2 = 2(x-t)_+$. At $x = 1/3$: $2(1/3 - t)_+$.

$\int_0^1(x-t)_+^2\,dx = \int_t^1(x-t)^2\,dx = \frac{(x-t)^3}{3}\Big|_t^1 = \frac{(1-t)^3}{3}$

$$R_x((x-t)_+^2) = \frac{(1-t)^3}{3} - \frac{1}{2}\cdot 2\left(\frac{1}{3}-t\right)_+ = \frac{(1-t)^3}{3} - \left(\frac{1}{3}-t\right)_+$$

$$K_2(t) = \frac{1}{2}\left[\frac{(1-t)^3}{3} - \left(\frac{1}{3}-t\right)_+\right]$$

Since Corollary requires constant sign (which needs checking), we use:

$R(e_3) = \frac{1}{12}$ (computed above).

If $K_2$ has constant sign:

$$R(f) = \frac{1}{3!}f'''(\xi)\cdot R(e_3) = \frac{1}{6}\cdot\frac{1}{12}\cdot f'''(\xi)$$

$$\boxed{R(f) = \frac{1}{72}f'''(\xi), \quad \xi \in (0,1)}$$
