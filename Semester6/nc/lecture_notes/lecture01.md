# Lecture 1 — Preliminaries: Taylor Polynomials, Errors, Stability, and Divided/Finite Differences

## Overview
This lecture sets up the foundations of Numerical Analysis: it reviews Taylor's theorem (the first example of a numerical method), classifies numerical problems, and develops the theory of errors (sources, propagation, significant digits). It then introduces stability and conditioning, and finally starts the study of divided differences, which underpin many later iterative and interpolation procedures.

## Key Notions & Definitions

- **Numerical Analysis** — a branch of Applied Mathematics that produces methods and procedures for finding numerical solutions to problems with a given precision (approximation of problems by simpler ones, algorithm construction, iteration methods, error analysis, stability, machine arithmetic).

- **Properties of an approximating procedure** — it should be (1) *convergent* (the sequence of successive approximations converges to the exact solution), (2) *stable* (small variations of input data lead to small variations in results), and (3) allow estimation of the *error* and *speed of convergence*.

- **Taylor's polynomial of degree $n$ of $f$ at $x_0$** — the polynomial
$$T_n(x) = f(x_0) + \frac{(x - x_0)}{1!} f'(x_0) + \cdots + \frac{(x - x_0)^n}{n!} f^{(n)}(x_0).$$

- **Taylor remainder (error)** — the term
$$R_n(x) = \frac{1}{n!} \int_{x_0}^{x} (x - t)^n f^{(n+1)}(t)\, dt = \frac{(x - x_0)^{n+1}}{(n+1)!} f^{(n+1)}(\xi), \quad \xi \text{ between } x \text{ and } x_0.$$

- **Types of numerical problems** — every numerical problem has the form $f(x) = y$:
  - **Direct (evaluation) problem** — $x$ and $f$ given, seek $y$.
  - **Inverse problem** — $y$ and $f$ given, seek $x$.
  - **Identifying problem** — $x$ and $y$ given, $f$ must be determined.

- **Approximation** — for $x \in \mathbb{R}$, a number $x^*$ is an approximation of $x$ (written $x \approx x^*$) if $x^* \in \mathcal{A}(x)$, where $\mathcal{A}$ is the approximating procedure/method.

- **Error (absolute error)** — $\Delta x = x - x^*$. If $\Delta x > 0$, $x^*$ is an *under-approximation*; if $\Delta x < 0$, an *over-approximation*.

- **Relative error** — $\delta x = \dfrac{\Delta x}{x}$, $x \neq 0$; in practice (since $x$ is unknown) $\delta x = \dfrac{\Delta x}{x^*}$. From this, $x^* = (1 - \delta x)x$.

- **Sources of error** — mathematical modeling of a physical problem; blunders (human/programming errors); uncertainty in physical data; machine errors (rounding/chopping, underflow/overflow from finite floating-point mantissa); mathematical truncation error (approximating infinite processes by finite ones).

- **Floating-point representation** — a number $r$ in base $b$ (even) is written $r = \pm r_0 r_1 \dots r_p \cdot r_{p+1} \dots r_k \times b^e$, where $r_0, \dots, r_k$ form the *mantissa (significand)*, $e$ is the *exponent*, $b$ the *base*. Normalized means $r_0 \neq 0$.

- **Significant digits** — the digits $0, 1, \dots, b-1$ in a number's representation that are non-zero, or located between non-zero digits, or preceded by at least one non-zero digit.

- **Correct approximation with $n$ significant digits** — for $r = r_0 10^k + r_1 10^{k-1} + \cdots$, the number $r^* = r_0^* 10^k + \cdots + r_{n-1}^* 10^{k-n+1}$ approximates $r$ correctly with $n$ significant digits if
$$|\Delta r^*| \leq \frac{1}{2} \times 10^{k-n+1}.$$

- **Condition number** — a single number measuring how sensitive a map $f : \mathbb{R}^m \to \mathbb{R}^n$ is to small perturbations of $x$. It is $(\operatorname{cond} f)(x) = \|\Gamma(x)\|$ for a matrix norm, where $\Gamma(x) = [\gamma_{ij}]$ is the **conditioning matrix** with $\gamma_{ij} = \dfrac{x_i \frac{\partial f_j}{\partial x_i}}{f_j(x)}$.

- **Ill-conditioned problem** — one where $(\operatorname{cond} f)(x) \gg 1$, so small relative input errors cause large output errors.

- **Ill-posed problem** — one whose result depends discontinuously on continuously varying data (e.g. computing an integer result from real data); cannot be solved accurately near the discontinuity even with exact input.

- **First-order divided difference** — for distinct nodes $x_0, x_1$,
$$f[x_0, x_1] = \frac{f(x_1) - f(x_0)}{x_1 - x_0}.$$
It is a *discrete version of the derivative*. With $f[x_0] = f(x_0)$ as the order-0 divided difference.

- **Divided difference of order $n$** — defined recursively for distinct nodes $x_0, \dots, x_n$:
$$f[x_0, x_1, \dots, x_n] = \frac{f[x_1, x_2, \dots, x_n] - f[x_0, x_1, \dots, x_{n-1}]}{x_n - x_0}.$$
The denominator is the difference of the nodes *not* common to the two numerator differences.

- **Divided difference with multiple (coincident) nodes** — for a node $x_0$ of multiplicity $n+1$,
$$f[\underbrace{x_0, x_0, \dots, x_0}_{n+1}] = \frac{f^{(n)}(x_0)}{n!},$$
the limiting case $f[x_0, x_0] = \lim_{x_1 \to x_0} f[x_0, x_1] = f'(x_0)$.

## Theorems & Results

### Theorem 1.1 (Taylor's Theorem)
Let $f : [a, b] \to \mathbb{R}$ have $n+1$ continuous derivatives on $[a,b]$, $n \geq 0$, and let $x, x_0 \in [a, b]$. Then
$$f(x) = T_n(x) + R_n(x),$$
with $T_n$ the Taylor polynomial and $R_n$ the remainder (defined above). This is the central tool: Taylor polynomials of higher degree give better and better approximations (series representations follow by letting $n \to \infty$, valid for all $x$ for $e^x, \cos x, \sin x$ and for $|x| < 1$ for $(1+x)^a$ and $\frac{1}{1-x}$).

### Theorem 1.4 (Taylor's formula in two dimensions)
Let $f : D \subset \mathbb{R}^2 \to \mathbb{R}^2$ be $n+1$ times continuously differentiable on $D$, $(x,y), (x_0,y_0) \in D$. Then $f(x,y) = T_n(x,y) + R_n(x,y)$, where
$$T_n(x,y) = f(x_0,y_0) + \frac{x - x_0}{1!} f'_x(x_0,y_0) + \frac{y - y_0}{1!} f'_y(x_0,y_0) + \sum_{j=1}^{n} \frac{1}{j!}\left[(x - x_0)\frac{\partial}{\partial x} + (y - y_0)\frac{\partial}{\partial y}\right]^j f(x,y)\Big|_{\substack{x = x_0 \\ y = y_0}}$$
and the remainder uses the same operator raised to power $n+1$ evaluated at an intermediate point $(\xi, \eta)$ on the segment between $(x,y)$ and $(x_0,y_0)$.

### Propagation of errors (Case 1: errors in input → error in output)
Using two-variable Taylor expansion at $(x^*, y^*)$ and neglecting second-order terms $(\Delta x)^2, \Delta x \Delta y, (\Delta y)^2$, the **maximum absolute error** of $f$ is
$$\Delta f \simeq \Delta x\, f'_x(x^*, y^*) + \Delta y\, f'_y(x^*, y^*).$$
In general, for $n$ variables: $\Delta f \simeq \sum_{i=1}^{n} \Delta x_i\, f'_{x_i}()$. The relative (propagated) error is
$$\delta f = \frac{\Delta f}{f} \simeq \sum_{i=1}^{n} x_i \left(\frac{d}{dx_i} \ln f()\right) \delta x_i.$$

### Inverse error problem (Case 2: principle of equal effects)
To guarantee a preset output accuracy, assume all terms $f'_{x_i} \Delta x_i$ have the *same effect*: $f'_{x_1}\Delta x_1 = \cdots = f'_{x_n}\Delta x_n$. Then $\Delta f \simeq n \Delta x_i f'_{x_i}$, giving the required absolute error per input
$$\Delta x_i \simeq \frac{\Delta f}{n f'_{x_i}}, \qquad \delta x_i \simeq \frac{\delta f}{n x_i \frac{d}{dx_i}(\ln f)}.$$

### Relation between significant digits and relative error
If $r^*$ is the correct approximation with $n$ significant digits (normalized representation $r^* = r_0^* 10^k + \cdots + r_{n-1}^* 10^{k-n+1}$), then the *maximum relative error* satisfies
$$\delta r^* \leq \frac{1}{r_0^* \times 10^{n-1}}.$$

### Condition number — particular cases ($m = n = 1$)
- $x \neq 0, f(x) \neq 0$: $(\operatorname{cond} f)(x) = \left|\dfrac{x f'(x)}{f(x)}\right|$.
- $x = 0, f(x) \neq 0$: $(\operatorname{cond} f)(x) = \left|\dfrac{f'(x)}{f(x)}\right|$.
- $x \neq 0, f(x) = 0$: $(\operatorname{cond} f)(x) = |x f'(x)|$.
- $x = f(x) = 0$: $(\operatorname{cond} f)(x) = |f'(x)|$.

For a linear function: $(\operatorname{cond} f)(x) = \dfrac{\|x\| \left\|\frac{\partial f}{\partial x}\right\|}{\|f(x)\|}$.

## Example Problems

### Example: Taylor series of elementary functions ($x_0 = 0$)
Using **Taylor's Theorem (1.1)** at $x_0 = 0$:
$$e^x = 1 + x + \frac{x^2}{2!} + \cdots + \frac{x^n}{n!} + \frac{x^{n+1}}{(n+1)!}e^{\xi_x},$$
$$\cos x = 1 - \frac{x^2}{2!} + \frac{x^4}{4!} - \cdots + (-1)^n \frac{x^{2n}}{(2n)!} + (-1)^{n+1}\frac{x^{2n+2}}{(2n+2)!}\cos\xi_x,$$
$$\frac{1}{1-x} = 1 + x + x^2 + \cdots + x^n + \frac{x^{n+1}}{1-x}.$$
This illustrates how the remainder $R_n$ controls the approximation: higher degree shrinks the error (Fig. 1 and Fig. 2 in the lecture show degrees 1–4 approximating $e^x$ on $[-1,1]$, with errors decreasing as degree increases).

### Example: Significant digits of an approximation of $e$ (Example 3.4)
Find how many significant digits $e^* = 2.718282$ shares with $e = 2.71828182\dots$. Write $e^* = 2\cdot 10^0 + 7\cdot 10^{-1} + 1 \cdot 10^{-2} + \cdots$, so $k = 0$. Then
$$\Delta e^* = e^* - e = 0.00000018 = 0.18 \times 10^{-6} \leq \frac{1}{2}\times 10^{-6}.$$
Comparing to the definition $|\Delta r^*| \leq \frac{1}{2}\times 10^{k-n+1}$ gives $10^{k-n+1} = 10^{-6} \Rightarrow 0 - n + 1 = -6 \Rightarrow n = 7$. So $e^*$ is correct to **7 significant digits**. This directly uses *Definition 3.3* above.

### Example: Conditioning of recurrence relations (Example 4.1.2)
For $I_n = \int_0^1 \frac{t^n}{t+5}\,dt$, the *direct recurrence* $I_n = \frac{1}{n} - 5 I_{n-1}$ has $(\operatorname{cond} f_n)(I_0) = \mathcal{O}(5^n)$ → **ill-conditioned**, while the *inverse recurrence* $I_n = \frac{1}{5}\left(\frac{1}{n+1} - 5 I_{n+1}\right)$ has $(\operatorname{cond} g_n)(I_{100}) = \mathcal{O}(5^{100-n})$ → **well-conditioned**. This illustrates the *condition number* concept: the same mathematical quantity can be computed by a stable or an unstable scheme.

### Example: Divided difference table for $f(x) = \sin\pi x$ (Example 5.5)
Nodes $x_0 = 0,\ x_1 = \tfrac{1}{6},\ x_2 = \tfrac{1}{2}$, so $f[x_0] = 0$, $f[x_1] = \tfrac{1}{2}$, $f[x_2] = 1$. Using the recursive **Definition 5.3**:
$$f[x_0, x_1] = \frac{1/2 - 0}{1/6 - 0} = 3, \qquad f[x_1, x_2] = \frac{1 - 1/2}{1/2 - 1/6} = \frac{3}{2},$$
$$f[x_0, x_1, x_2] = \frac{3/2 - 3}{1/2 - 0} = -3.$$
The first row of the table holds all the divided differences needed for interpolation.

### Example: Divided differences with multiple nodes (Example 5.6 / 5.7)
For a double node $x_0$ and simple node $x_1$, set $z_0 = z_1 = x_0$, $z_2 = x_1$. Then $f[z_0, z_1] = f[x_0,x_0] = f'(x_0)$ (the *coincident-node rule* $f[\underbrace{x_0,\dots,x_0}_{n+1}] = f^{(n)}(x_0)/n!$), while $f[z_1, z_2] = \frac{f(x_1) - f(x_0)}{x_1 - x_0}$ and the order-2 difference is $\dfrac{\frac{f(x_1)-f(x_0)}{x_1 - x_0} - f'(x_0)}{x_1 - x_0}$. For 3 double nodes, repeated first-order differences with coincident nodes are replaced by $f'(x_i)$. This shows how the multiple-node definition extends the ordinary divided-difference table.

## Cheat-sheet
- Taylor: $f(x) = T_n(x) + R_n(x)$, $R_n = \frac{(x-x_0)^{n+1}}{(n+1)!}f^{(n+1)}(\xi)$.
- Absolute error $\Delta x = x - x^*$; relative error $\delta x = \Delta x / x$ (or $/x^*$).
- Error propagation: $\Delta f \simeq \sum_i \Delta x_i f'_{x_i}$; $\delta f \simeq \sum_i x_i \frac{d}{dx_i}(\ln f)\,\delta x_i$.
- Equal-effects (inverse): $\Delta x_i \simeq \frac{\Delta f}{n f'_{x_i}}$.
- $n$ correct significant digits $\iff |\Delta r^*| \leq \frac{1}{2}\times 10^{k-n+1}$; then $\delta r^* \leq \frac{1}{r_0^* \times 10^{n-1}}$.
- Condition number $(\operatorname{cond} f)(x) = \|\Gamma(x)\|$; scalar case $\left|\frac{x f'(x)}{f(x)}\right|$.
- $\operatorname{cond} \gg 1$ → ill-conditioned; discontinuous dependence on data → ill-posed.
- Divided difference: $f[x_0,x_1] = \frac{f(x_1)-f(x_0)}{x_1-x_0}$; order $n$: $f[x_0,\dots,x_n] = \frac{f[x_1,\dots,x_n] - f[x_0,\dots,x_{n-1}]}{x_n - x_0}$.
- Coincident nodes: $f[\underbrace{x_0,\dots,x_0}_{n+1}] = \frac{f^{(n)}(x_0)}{n!}$; in particular $f[x_0,x_0] = f'(x_0)$.
