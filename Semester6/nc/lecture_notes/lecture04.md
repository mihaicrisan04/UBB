# Lecture 4 — Polynomial Interpolation: Lagrange, Error, Chebyshev Nodes & Barycentric Formula

## Overview
This lecture opens Chapter 3 (Approximation of Functions) with polynomial interpolation: building a polynomial that passes through given data points and using it to approximate a function. It covers the Lagrange interpolation polynomial and its uniqueness, the interpolation error formula, why equispaced nodes fail (Runge's phenomenon) and how Chebyshev nodes fix it, and finally the barycentric formula for evaluating the interpolant efficiently and stably.

## Key Notions & Definitions

- **Interpolation** — finding and evaluating a function (here a polynomial) whose graph passes through a set of given points. Given $n+1$ distinct **nodes (knots)** $x_i\in[a,b]$, $i=\overline{0,n}$ and values $f(x_i)=y_i$, find a minimum-degree polynomial $P$ with $$P(x_i) = f(x_i),\quad i=\overline{0,n}$$ (the **interpolation conditions**).

- **Linear interpolation** — through two nodes $(x_0,y_0),(x_1,y_1)$: $$P_1(x) = y_0 + \frac{y_1-y_0}{x_1-x_0}(x-x_0) = \frac{x-x_1}{x_0-x_1}y_0 + \frac{x-x_0}{x_1-x_0}y_1.$$

- **Quadratic interpolation** — through three nodes: $$P_2(x) = \frac{(x-x_1)(x-x_2)}{(x_0-x_1)(x_0-x_2)}y_0 + \frac{(x-x_0)(x-x_2)}{(x_1-x_0)(x_1-x_2)}y_1 + \frac{(x-x_0)(x-x_1)}{(x_2-x_0)(x_2-x_1)}y_2.$$

- **Node polynomial** — $u(x) = \prod_{j=0}^n (x-x_j) = (x-x_0)(x-x_1)\dots(x-x_n)$, with $u_j(x) = \dfrac{u(x)}{x-x_j}$.

- **Lagrange fundamental (basis) polynomials** — $$l_i(x) = \prod_{\substack{j=0\\ j\neq i}}^{n}\frac{x-x_j}{x_i-x_j} = \frac{u_i(x)}{u_i(x_i)} = \frac{u_i(x)}{u'(x_i)},$$ satisfying $l_i(x_j) = \delta_{ij}$.

- **Lagrange interpolation polynomial** — $$L_n f(x) = \sum_{i=0}^n l_i(x) f(x_i).$$

- **Remainder (interpolation error)** — $(R_n f)(x) = f(x) - (L_n f)(x)$, $x\in[a,b]$.

- **Chebyshev polynomial of the first kind** — $T_n(x) = \cos(n\arccos x)$, $x\in[-1,1]$. A degree-$n$ polynomial with leading coefficient $2^{n-1}$.

- **Chebyshev polynomial of the second kind** — $Q_n(x) = \dfrac{\sin((n+1)\arccos x)}{\sqrt{1-x^2}}$, $x\in(-1,1)$.

- **Barycentric weights** — $w_i = \dfrac{1}{u'(x_i)} = \dfrac{1}{\prod_{j\neq i}(x_i-x_j)}$, $i=0,\dots,n$.

- **First barycentric formula** (modified Lagrange) — $$L_n f(x) = u(x)\sum_{i=0}^n \frac{w_i}{x-x_i}f_i.$$

- **Second barycentric formula** (the barycentric formula) — $$L_n f(x) = \frac{\displaystyle\sum_{i=0}^n \frac{w_i}{x-x_i}f_i}{\displaystyle\sum_{i=0}^n \frac{w_i}{x-x_i}}.$$

## Theorems & Results

### Theorem 1.3 — existence and uniqueness of the Lagrange interpolant
There is a unique polynomial $L_n f$ of degree at most $n$ satisfying the interpolation conditions, given by $L_n f(x) = \sum_{i=0}^n l_i(x)f(x_i)$.

**Proof sketch.** Each $l_i$ has degree $\le n$ and satisfies $l_i(x_j)=\delta_{ij}$, so $L_n f$ has degree $\le n$ and meets the interpolation conditions. For uniqueness, suppose another degree-$\le n$ polynomial $P_n^*$ also interpolates; then $Q_n = L_n - P_n^*$ has degree $\le n$ but vanishes at the $n+1$ distinct nodes, so by the Fundamental Theorem of Algebra $Q_n\equiv 0$. (Note: the actual degree may be less than $n$.)

### Theorem 1.5 — interpolation error formula
Let $f\in C^{n+1}[a,b]$ and let $\{x_0,\dots,x_n\}\subset[a,b]$ be distinct. Then there exists $\xi\in(a,b)$ with
$$(R_n f)(x) = \frac{u(x)}{(n+1)!}f^{(n+1)}(\xi),\quad u(x)=(x-x_0)\dots(x-x_n).$$

### Remark 1.7 — upper bound and convergence
An upper bound for the error is $$|(R_n f)(x)| \le \frac{|u(x)|}{(n+1)!}M_{n+1}(f),\quad M_{n+1}(f) = \sup_{t\in[a,b]}|f^{(n+1)}(t)|.$$
Convergence of $L_n f$ to $f$ does **not** hold in general. It holds if $f\in C^\infty[a,b]$ with $|f^{(k)}(x)|\le M_k$ and $\lim_{k\to\infty}\frac{(b-a)^k}{k!}M_k = 0$. Bernstein and Faber proved (early 1900s) that for every triangular array of nodes there is a continuous $f$ whose Lagrange interpolants do not converge uniformly to $f$.

### Chebyshev nodes minimize the error term $|u(x)|$
On $[-1,1]$, choosing $u(x) = \tilde T_{n+1}(x) = \frac{1}{2^n}T_{n+1}(x)$ minimizes $\|u\|_\infty$. This gives the nodes (roots of $T_{n+1}$)
$$x_k = \cos\!\left(\frac{2k+1}{2n+2}\pi\right),\quad k=0,\dots,n,$$
with error bound $\|R_n f\| \le \dfrac{1}{2^n(n+1)!}\|f^{(n+1)}\|$. For a general $[a,b]$ (via $x = \frac{b-a}{2}t + \frac{b+a}{2}$), $\|R_n f\| \le \dfrac{(b-a)^{n+1}}{2^{2n+1}(n+1)!}\|f^{(n+1)}\|$.

### Properties of Chebyshev polynomials of the first kind
Recurrence $T_{k+1}(x) = 2x\,T_k(x) - T_{k-1}(x)$, with $T_0=1$, $T_1=x$, $T_2=2x^2-1$, $T_3=4x^3-3x$, $T_4=8x^4-8x^2+1$. They are orthogonal on $(-1,1)$ with weight $w(x)=\frac{1}{\sqrt{1-x^2}}$:
$$\int_{-1}^{1}\frac{T_n(x)T_m(x)}{\sqrt{1-x^2}}\,dx = \begin{cases}0, & n\neq m\\ \pi, & n=m=0\\ \pi/2, & n=m\neq 0\end{cases}.$$

### Properties of Chebyshev polynomials of the second kind
Roots $x_k = \cos\!\left(\frac{k}{n+1}\pi\right)$, $k=1,\dots,n$. Recurrence $Q_{k+1}=2xQ_k - Q_{k-1}$ with $Q_0=1$, $Q_1=2x$ (so $Q_2=4x^2-1$, $Q_3=8x^3-4x$). Orthogonal on $[-1,1]$ with weight $w(x)=\sqrt{1-x^2}$. Mutual relations: $T_{n+1}=xT_n-(1-x^2)Q_{n-1}$ and $Q_{n+1}=xQ_n+T_{n+1}$.

### Efficient evaluation — barycentric interpolation
The plain Lagrange formula costs $O(n^2)$ flops per evaluation point, requires recomputation when a node is added, and is numerically unstable. The barycentric formula fixes this: precompute the weights $w_i$ once in $O(n^2)$, then each evaluation of $L_n f(x)$ costs $O(n)$. Adding a node $x_{n+1}$ costs only $O(n)$ (divide each $w_i$ by $x_i-x_{n+1}$ and compute $w_{n+1}$). The second barycentric formula is technically undefined at a node $x_k$, but by L'Hopital $\lim_{x\to x_k}L_n f(x) = f_k$, so a continuous extension is used.

### Explicit barycentric weights for special node sets
- Equispaced nodes on $[-1,1]$ (any $[a,b]$, common factors dropped): $w_j = (-1)^j\binom{n}{j}$, $j=0,\dots,n$. These vary by factors $\approx 2^n$, which is the source of Runge's phenomenon.
- Chebyshev points of the first kind: $w_j = (-1)^j\sin\frac{(2j+1)\pi}{2n+2}$. These vary only by $O(n)$ factors, making interpolation well-conditioned.
- Chebyshev points of the second kind: $w_j = (-1)^j\cdot\tfrac12$ if $j=0$ or $j=n$, and $w_j=(-1)^j$ otherwise.

## Example Problems

### Example: Linear interpolation of $\sqrt{x}$ (Example 1.1)
Take $f(x)=\sqrt{x}$ with nodes $x_0=1$, $x_1=4$, i.e. data $(1,1),(4,2)$.

**Solution.** Using the linear interpolation formula above,
$$P_1(x) = \frac{x-4}{1-4}\cdot 1 + \frac{x-1}{4-1}\cdot 2 = \frac{1}{3}x + \frac{2}{3}.$$
This is the $n=1$ case of the Lagrange interpolant (Theorem 1.3).

### Example: Quadratic interpolation of $\sqrt{x}$ (Example 1.2)
Add the node $(9,3)$ to the previous data.

**Solution.** With nodes $(1,1),(4,2),(9,3)$ the quadratic Lagrange polynomial is
$$P_2(x) = \frac{(x-4)(x-9)}{(1-4)(1-9)}\cdot 1 + \frac{(x-1)(x-9)}{(4-1)(4-9)}\cdot 2 + \frac{(x-1)(x-4)}{(9-1)(9-4)}\cdot 3 = -\frac{1}{60}x^2 + \frac{5}{12}x + \frac{3}{5}.$$
This uses the quadratic interpolation formula / Theorem 1.3; $P_2$ tracks $\sqrt{x}$ more closely than $P_1$ on $[0,15]$.

### Example: Minimum-degree interpolant of 6 points (Example 1.4)
Find the minimum-degree polynomial interpolating
$$\begin{array}{c|cccccc} x & -2 & -1 & 0 & 1 & 2 & 3 \\\hline y & -5 & 1 & 1 & 1 & 7 & 25\end{array}$$

**Solution.** Applying the Lagrange formula $L_5 f(x) = \sum_{i=0}^5 l_i(x)y_i$ and simplifying gives
$$L_5 f(x) = x^3 - x + 1,$$
which has degree $3 < 5$. This illustrates the remark after Theorem 1.3: with $n+1$ nodes the interpolant has degree at most $n$, but can be strictly smaller.

### Example: interpolation error for $\sqrt{x}$ (Example 1.6)
**Solution.** For $f(x)=\sqrt{x}=x^{1/2}$: $f''(x) = -\tfrac14\cdot\frac{1}{x\sqrt{x}}$ and $f'''(x) = \tfrac38\cdot\frac{1}{x^2\sqrt{x}}$. By Theorem 1.5 the linear and quadratic remainders are
$$(R_1 f)(x) = \frac{(x-x_0)(x-x_1)}{2}f''(\xi),\qquad (R_2 f)(x) = \frac{(x-x_0)(x-x_1)(x-x_2)}{6}f'''(\xi),$$
giving the bounds $|(R_1 f)(x)| = \frac{|(x-x_0)(x-x_1)|}{8}\cdot\frac{1}{\xi\sqrt{\xi}}$ and $|(R_2 f)(x)| = \frac{|(x-x_0)(x-x_1)(x-x_2)|}{16}\cdot\frac{1}{\xi^2\sqrt{\xi}}$. This is a direct application of the error formula (Theorem 1.5).

### Example: Bernstein's and Runge's examples (Examples 1.8, 1.9)
**Bernstein:** $f(x)=|x|$ on $[-1,1]$ with equidistant nodes $x_k^{(n)} = -1 + \frac{2k}{n}$ gives $\lim_{n\to\infty}|f(x)-L_n f(x)| = \infty$ for every $x$ except $x=-1,0,1$. **Runge:** $f(x)=\frac{1}{1+x^2}$ on $[-5,5]$ with equally spaced nodes diverges for $|x|>3.633\dots$ and converges only for $|x|<3.633\dots$. Both illustrate **Runge's phenomenon** (Remark 1.7): polynomial interpolation on equispaced nodes is highly ill-conditioned, with large oscillations near the endpoints.

### Example: Bernstein revisited with Chebyshev nodes (Example 1.10)
Interpolating $f(x)=|x|$ on $[-1,1]$ with the Chebyshev nodes $x_k=\cos\frac{2k+1}{2n+2}\pi$ instead of equispaced ones produces a far better-behaved interpolant (no edge blow-up). This demonstrates that Chebyshev nodes minimize $|u(x)|$ in the error formula and make interpolation well-conditioned.

### Example: barycentric form for $\sqrt{x}$ (Example 1.12)
With nodes $x_0=1$, $x_1=4$, the barycentric weights are $w_0 = \frac{1}{x_0-x_1} = -\frac13$ and $w_1 = \frac{1}{x_1-x_0} = \frac13$. The second barycentric formula gives
$$L_1 f(x) = \frac{\frac{w_0}{x-x_0}f_0 + \frac{w_1}{x-x_1}f_1}{\frac{w_0}{x-x_0}+\frac{w_1}{x-x_1}} = \frac{1}{3}x + \frac{2}{3},$$
exactly $P_1$ from Example 1.1. This shows (Remark 1.13) that despite its quotient form, the barycentric formula really is the same degree-$1$ polynomial.

## Cheat-sheet
- Interpolation conditions: $P(x_i)=f(x_i)$, $i=\overline{0,n}$; unique degree-$\le n$ solution.
- Lagrange basis: $l_i(x)=\prod_{j\neq i}\frac{x-x_j}{x_i-x_j}$, $l_i(x_j)=\delta_{ij}$; $L_n f=\sum_i l_i f(x_i)$.
- Linear: $P_1 = \frac{x-x_1}{x_0-x_1}y_0 + \frac{x-x_0}{x_1-x_0}y_1$.
- Error: $(R_n f)(x) = \frac{u(x)}{(n+1)!}f^{(n+1)}(\xi)$, $u(x)=\prod_i(x-x_i)$; bound $\frac{|u(x)|}{(n+1)!}M_{n+1}(f)$.
- Equispaced high-degree interpolation $\Rightarrow$ Runge's phenomenon (ill-conditioned, edge oscillation).
- Chebyshev (1st kind) nodes: $x_k=\cos\frac{2k+1}{2n+2}\pi$; minimize $|u(x)|$; error $\le \frac{1}{2^n(n+1)!}\|f^{(n+1)}\|$.
- $T_n(x)=\cos(n\arccos x)$; recurrence $T_{k+1}=2xT_k-T_{k-1}$; $T_0=1,T_1=x,T_2=2x^2-1,T_3=4x^3-3x$.
- $Q_n(x)=\frac{\sin((n+1)\arccos x)}{\sqrt{1-x^2}}$; $Q_0=1,Q_1=2x$; roots $\cos\frac{k}{n+1}\pi$.
- Barycentric weights $w_i = 1/\prod_{j\neq i}(x_i-x_j)$.
- Second barycentric formula: $L_n f(x) = \frac{\sum_i \frac{w_i}{x-x_i}f_i}{\sum_i \frac{w_i}{x-x_i}}$; $O(n)$ per evaluation, $O(n)$ to add a node.
- Equispaced weights $w_j=(-1)^j\binom{n}{j}$ (vary by $\sim 2^n$); Chebyshev-1st $w_j=(-1)^j\sin\frac{(2j+1)\pi}{2n+2}$ (vary by $O(n)$).
