# Lecture 9 — Numerical Differentiation and Integration (Newton–Cotes Quadratures)

## Overview
This lecture opens Chapter 4 on numerical differentiation and integration. It sets up the general framework of approximating a linear functional $L$ by other functionals, then derives finite-difference formulas for derivatives and the three classical Newton–Cotes quadrature rules (rectangle/midpoint, trapezoidal, Simpson), including their errors via Peano's theorem and their composite versions. It closes with adaptive quadrature. These methods matter because they let us differentiate/integrate functions known only at sample points or whose closed form is intractable.

## Key Notions & Definitions
- **Approximation formula of a functional** — for real linearly independent functionals $L, L_1, \dots, L_m : X \to \mathbb{R}$, a formula
  $$L(f) = \sum_{i=1}^{m} A_i L_i(f) + R(f), \quad f \in X,$$
  where $A_i$ are the **coefficients** and $R(f)$ is the **remainder**. The $L_i$ encode the available information on $f$.
- **Lagrange interpolation as an approximation formula** — taking $L_i(f) = f(x_i)$ and $L(f) = f(\alpha)$ gives $f(\alpha) = \sum_{i=0}^m l_i(\alpha) f(x_i) + (Rf)(\alpha)$ with $A_i = l_i(\alpha)$ and remainder $(Rf)(\alpha) = \frac{u(\alpha)}{(m+1)!} f^{(m+1)}(\xi)$, $u(x) = (x-x_0)\cdots(x-x_m)$.
- **Numerical differentiation formula** — approximation of a derivative of order $k$: $f^{(k)}(\alpha) = \sum_{i=0}^m A_i f(x_i) + R(f)$.
- **Numerical integration (quadrature) formula** — $\int_a^b f(x)\,dx = \sum_{k=0}^m \sum_{j \in I_k} A_{kj} f^{(j)}(x_k) + R(f)$; with weight $w$ it reads $\int_a^b w(x) f(x)\,dx = \sum_{j=0}^m A_j F_j(f) + R(f)$.
- **Two solution approaches** — the **interpolation method** (apply $L$ to an interpolation polynomial of $f$) and the **method of undetermined coefficients** (choose coefficients so $R(e_k)=0$, $e_k(x)=x^k$, for $k=0,\dots,d$ as large as possible).
- **Forward difference derivative** — $f'(x) \approx \frac{f(x+h)-f(x)}{h} \equiv D_h f(x)$.
- **Backward difference derivative** — $f'(x) \approx \frac{f(x)-f(x-h)}{h} \equiv \widetilde{D}_h f(x)$.
- **Central difference derivative** — $f'(x) \approx \frac{f(x+h)-f(x-h)}{2h} \equiv \widehat{D}_h f(x)$ (obtained by interpolating at $x-h, x+h$ and differentiating).
- **Degree of precision (exactness)** — the natural number $d$ such that $R(f)=0$ for all $f \in \mathbb{P}_d$ but $R(g)\neq 0$ for some $g \in \mathbb{P}_{d+1}$. Equivalently $R(e_j)=0$ for $j=0,\dots,d$ and $R(e_{d+1})\neq 0$.
- **Interpolatory quadrature** — quadrature $\int_a^b f\,dx = \sum_{k=0}^m A_k f(x_k) + R(f)$ obtained by integrating an interpolation polynomial; has degree of precision at least $m$.
- **Newton–Cotes quadrature** — interpolatory quadrature with equally spaced nodes; **closed** if it includes the endpoints $a,b$, otherwise **open**. Maximum degree of precision with $m+1$ nodes is $d_{\max} = 2m+1 = 2\cdot(\text{nr. nodes}) - 1$.
- **Weight function** — integrable $w : [a,b] \to \mathbb{R}_+$ used to absorb singularities of the integrand.
- **Adaptive quadrature** — a divide-and-conquer scheme that compares two methods on a subinterval and recursively subdivides only where the discrepancy is large, computing only as much work as needed.

## Theorems & Results

### Forward difference error (2.2)
Taylor expansion $f(x+h) = f(x) + hf'(x) + \tfrac{h^2}{2}f''(\xi)$ gives
$$(RD_h f)(x) = f'(x) - D_h f(x) = -\tfrac{h}{2} f''(\xi), \quad \xi \in (x, x+h).$$
Error is $O(h)$. The backward difference has the analogous error $(R\widetilde{D}_h f)(x) = \tfrac{h}{2} f''(\xi)$, $\xi \in (x-h,x)$.

### Central difference error (2.6)
$$(R\widehat{D}_h f)(x) = -\tfrac{h^2}{6} f'''(\xi), \quad \xi \in (x-h, x+h).$$
This is $O(h^2)$, so for small $h$ the central formula is more accurate than the one-sided ones.

### Instability of numerical differentiation (Remark 2.2)
Numerical differentiation is an **unstable** operation: a good approximation of $f$ does not guarantee a good approximation of $f'$. Example: $f = g + \frac{x^{n^2}}{n}$ on $[0,1]$ has $\|f-g\|_\infty = \tfrac{1}{n} \to 0$ but $\|f'-g'\|_\infty = n \to \infty$.

### Rectangle (midpoint) rule (3.4)–(3.5)
Interpolating $f$ at the single double node $x_0 = \tfrac{a+b}{2}$ (Taylor polynomial of degree 1) and integrating:
$$\int_a^b f(x)\,dx = (b-a)\, f\!\left(\tfrac{a+b}{2}\right) + R(f), \qquad R(f) = \frac{(b-a)^3}{24} f''(\xi), \quad \xi \in (a,b).$$
Open Newton–Cotes formula, degree of precision $d=1$ (maximum for one node). Proof of $d=1$: $R(e_0)=R(e_1)=0$ but $R(e_2) = \frac{(b-a)^3}{12} \neq 0$.

### Peano's theorem for quadratures
For $\int_a^b f\,dx = Q(f) + R(f)$ with degree of precision $d=n$ and $f \in C^{n+1}[a,b]$, the remainder is
$$R(f) = \int_a^b K_n(t) f^{(n+1)}(t)\,dt, \qquad K_n(t) = \frac{1}{n!}\left[\frac{(b-t)_+^{n+1} - (a-t)_+^{n+1}}{n+1} - Q\big((x-t)_+^n\big)\right].$$
If $K_n$ has constant sign on $[a,b]$, then $R(f) = \frac{1}{(n+1)!} f^{(n+1)}(\xi)\, R(e_{n+1})$, $\xi \in (a,b)$.

### Trapezoidal rule (3.7)
Integrating the Lagrange interpolant at $x_0=a, x_1=b$:
$$\int_a^b f(x)\,dx = \frac{b-a}{2}\big(f(a)+f(b)\big) - \frac{(b-a)^3}{12} f''(\xi), \quad \xi \in (a,b).$$
Closed Newton–Cotes formula, degree of precision $d=1$ (since $R(e_2) = -\frac{(b-a)^3}{6}\neq 0$).

### Simpson's (Cavalieri–Simpson) rule (3.9)
From Hermite interpolation at $x_0=a$ (simple), $x_1=\tfrac{a+b}{2}$ (double), $x_2=b$ (simple), giving a degree-3 polynomial:
$$\int_a^b f(x)\,dx = \frac{b-a}{6}\left[f(a) + 4 f\!\left(\tfrac{a+b}{2}\right) + f(b)\right] - \frac{(b-a)^5}{2880} f^{(4)}(\xi), \quad \xi \in (a,b).$$
Degree of precision $d=3$. Hermite interpolation (rather than 3 simple nodes) is used so the remainder's integral does not vanish trivially.

### Composite formulas
Splitting $[a,b]$ into $n$ subintervals with $h = (b-a)/n$, $x_i = a+ih$:
- **Composite rectangle (3.6):** $\int_a^b f\,dx = h \sum_{i=0}^{n-1} f\!\big(a + (i+\tfrac12)h\big) + \frac{h^2(b-a)}{24} f''(\xi)$.
- **Composite trapezoidal (3.8):** $\int_a^b f\,dx = \frac{h}{2}\big[f(a) + 2(f_1 + \dots + f_{n-1}) + f(b)\big] - \frac{h^2(b-a)}{12} f''(\xi)$.
- **Composite Simpson (3.10)** (with $n=2m$ even, $h=\tfrac{b-a}{2m}$): $\int_a^b f\,dx = \frac{h}{3}\big[f(a) + 4\sum_{i=1}^{m} f_{2i-1} + 2\sum_{i=1}^{m-1} f_{2i} + f(b)\big] - \frac{h^4(b-a)}{180} f^{(4)}(\xi)$.

Precision-wise: rectangle and trapezoidal are $O(h^2)$; Simpson is $O(h^4)$ but more expensive. Doubling $n$ reuses previously computed values (Remark 3.5), favoring the trapezoidal rule.

### Adaptive quadrature (Algorithm 3.1)
```
function I = adquad(f, a, b, eps, met, m)
    I1 = met(f, a, b, m);
    I2 = met(f, a, b, 2m);
    if |I1 - I2| < eps          % success
        I = I2; return
    else                         % recursive subdivision
        I = adquad(f, a, (a+b)/2, eps, met, m) + adquad(f, (a+b)/2, b, eps, met, m);
    end
end
```
`met` is a composite rule (trapezoidal, Simpson, ...), `m` the number of subintervals.

## Example Problems

### Example: Numerical derivative of $\cos x$ at $\pi/6$ (Example 2.1)
Approximate $f'(\pi/6)$ for $f(x)=\cos x$ using $D_h f$ and $\widehat{D}_h f$. Exact value $f'(\pi/6) = -\sin\tfrac{\pi}{6} = -\tfrac12$. This illustrates the **forward** and **central difference errors** above: $|(RD_h f)(\pi/6)| \le \tfrac{h}{2}$ ($O(h)$) and $|(R\widehat{D}_h f)(\pi/6)| \le \tfrac{h^2}{6}$ ($O(h^2)$). Numerically, halving $h$ roughly halves the forward-difference error (ratio $\approx 2$), while it quarters the central-difference error (ratio $\approx 4$), confirming the respective rates.

### Example: Compare the three quadratures on $\int_0^1 \frac{dx}{1+x}$ (Example 3.7)
Exact value $\ln 2 = 0.693147\ldots$. This directly exercises the rectangle, trapezoidal, and Simpson rules derived above:
- Rectangle: $1 \cdot f(\tfrac12) = \tfrac23 = 0.6667$, error $0.0265$.
- Trapezoidal: $\tfrac12\big(f(0)+f(1)\big) = \tfrac34 = 0.75$, error $-0.0569$.
- Simpson: $\tfrac16\big[f(0) + 4f(\tfrac12) + f(1)\big] = \tfrac{25}{36} = 0.6944$, error $-0.0013$.
Simpson ($d=3$) is far more accurate, as expected from its higher degree of precision.

### Example: Composite trapezoidal vs Simpson on $\int_0^1 e^{-x^2}dx$ (Example 3.8)
With exact value $0.746824\ldots$, the composite rules show the predicted convergence: trapezoidal error ratio $\approx 4$ per doubling ($O(h^2)$), Simpson ratio $\approx 16$ ($O(h^4)$). This illustrates the composite error formulas (3.8) and (3.10).

### Example: Quadrature with a derivative node (Example 3.9)
Find $A,B$ in $\int_{-1}^1 f(x)\,dx = A f'(-1) + B f(1) + R(f)$ with maximal degree of exactness, then redo it via Birkhoff interpolation, and find the remainder. This uses the **method of undetermined coefficients** and **Peano's theorem**.
- (a) Impose $R(e_0)=2-B=0$, $R(e_1)=-A-B=0 \Rightarrow A=-2, B=2$. Check $R(e_2)=\tfrac23-6=-\tfrac{16}{3}\neq 0$, so $d=1$ and the formula is $\int_{-1}^1 f = 2\big(-f'(-1)+f(1)\big)+R(f)$.
- (b) Birkhoff interpolation with $x_0=-1, x_1=1$, $I_0=\{1\}, I_1=\{0\}$, degree $n=1$: $B_1 f(x) = x f'(-1) + (f(1)-f'(-1))$. Integrating gives $-2f'(-1)+2f(1)$, the same formula.
- (c) Since $d=1$, $R(f) = \int_{-1}^1 K_1(t) f''(t)\,dt$ with $K_1(t) = \tfrac12(1+t)^2 \ge 0$ of constant sign, so $R(f) = \tfrac12 f''(\xi) R(e_2) = -\tfrac83 f''(\xi)$, $\xi\in(-1,1)$. (Equivalently via Peano on the Birkhoff remainder $(R_1 f)(x) = \tfrac{(x-1)(x+3)}{2} f''(\xi)$.)

## Cheat-sheet
- General approximation: $L(f) = \sum A_i L_i(f) + R(f)$; coefficients $A_i$, remainder $R$.
- Forward: $D_h f = \frac{f(x+h)-f(x)}{h}$, error $-\tfrac{h}{2}f''(\xi)$, $O(h)$.
- Backward: $\widetilde{D}_h f = \frac{f(x)-f(x-h)}{h}$, error $\tfrac{h}{2}f''(\xi)$, $O(h)$.
- Central: $\widehat{D}_h f = \frac{f(x+h)-f(x-h)}{2h}$, error $-\tfrac{h^2}{6}f'''(\xi)$, $O(h^2)$.
- Degree of precision: $R(e_j)=0$, $j=0,\dots,d$, $R(e_{d+1})\neq 0$. Max for $m+1$ nodes: $2m+1$.
- Rectangle: $(b-a)f(\tfrac{a+b}{2})$, $R = \tfrac{(b-a)^3}{24} f''(\xi)$, $d=1$.
- Trapezoidal: $\tfrac{b-a}{2}(f(a)+f(b))$, $R = -\tfrac{(b-a)^3}{12} f''(\xi)$, $d=1$.
- Simpson: $\tfrac{b-a}{6}(f(a)+4f(\tfrac{a+b}{2})+f(b))$, $R = -\tfrac{(b-a)^5}{2880} f^{(4)}(\xi)$, $d=3$.
- Composite trapezoidal: $\tfrac{h}{2}[f(a)+2\sum_{i=1}^{n-1}f_i+f(b)]$, error $O(h^2)$.
- Composite Simpson: $\tfrac{h}{3}[f(a)+4\sum f_{2i-1}+2\sum f_{2i}+f(b)]$, error $O(h^4)$.
- Peano kernel: $R(f) = \int_a^b K_n(t) f^{(n+1)}(t)\,dt$; if $K_n$ has constant sign, $R(f) = \tfrac{1}{(n+1)!} f^{(n+1)}(\xi) R(e_{n+1})$.
- Adaptive quadrature: compare $\text{met}(m)$ vs $\text{met}(2m)$; subdivide if $|I_1-I_2|\ge\varepsilon$.
