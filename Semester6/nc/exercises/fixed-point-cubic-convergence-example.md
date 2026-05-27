# Fixed-Point Iteration with Cubic Convergence — Worked Example

**Problem.** Show that the polynomial $p(x) = 3x^4 - 8x^3 + 6x^2 - x$ has $\alpha = 1$ as a root. Does the iteration $x_{n+1} = 3x_n^4 - 8x_n^3 + 6x_n^2$ converge to $\alpha$ for a suitable $x_0$? Study the convergence, find the order of convergence, and give a bound for the error.

The iteration is $x_{n+1} = g(x_n)$ with $g(x) = 3x^4 - 8x^3 + 6x^2$. Note $p(x) = g(x) - x$, so the **roots of $p$ are exactly the fixed points of $g$.**

## 1. $\alpha = 1$ is a root (hence a fixed point)

$$p(1) = 3 - 8 + 6 - 1 = 0. \ \checkmark$$

Factoring, $p(x) = x(x-1)(3x^2 - 5x + 1)$, so $\alpha = 1$ is a **simple** root. Since $p = g - x$, this gives $g(1) = 1$: $\alpha = 1$ is a fixed point of the iteration.

## 2. Convergence for a suitable $x_0$

Differentiate at the fixed point:

$$g'(x) = 12x^3 - 24x^2 + 12x = 12x(x-1)^2, \qquad g'(1) = 0.$$

Since $|g'(1)| = 0 < 1$, the **local fixed-point theorem** guarantees convergence to $\alpha = 1$ for any $x_0$ sufficiently close to $1$.

Concretely, on $I = [0.9, 1.1]$: $\max_I |g'| = |g'(1.1)| = 12(1.1)(0.1)^2 = 0.132 < 1$, and $g(I) \subseteq I$ (e.g. $g(0.9) \approx 0.9963$, $g(1.1) \approx 1.0043$, $g(1) = 1$). So by the global fixed-point theorem the iteration converges for every $x_0 \in [0.9, 1.1]$.

## 3. Order of convergence

Keep differentiating and evaluate at $\alpha = 1$:

$$g'(1) = 0,$$
$$g''(x) = 36x^2 - 48x + 12 = 12(3x - 1)(x - 1) \;\Rightarrow\; g''(1) = 0,$$
$$g'''(x) = 72x - 48 \;\Rightarrow\; g'''(1) = 24 \neq 0.$$

The first non-vanishing derivative is the **third**, so by Theorem 3.3 the method has **order 3 (cubic)**, with asymptotic error constant

$$\lim_{n \to \infty} \frac{x_{n+1} - \alpha}{(x_n - \alpha)^3} = \frac{g'''(1)}{3!} = \frac{24}{6} = 4.$$

## 4. Error bound

With $e_n = x_n - \alpha$ and a Taylor expansion of $g$ about $\alpha$ (using $g'(\alpha) = g''(\alpha) = 0$):

$$e_{n+1} = \frac{g'''(\xi_n)}{3!}\, e_n^3 \;\Rightarrow\; |e_{n+1}| \le \frac{M_3}{6}\,|e_n|^3, \qquad M_3 = \max_I |g'''|.$$

On $I = [0.9, 1.1]$, $g'''(x) = 72x - 48$ gives $M_3 = |72(1.1) - 48| = 31.2$, so

$$\boxed{\,|e_{n+1}| \le \frac{31.2}{6}\,|e_n|^3 = 5.2\,|e_n|^3\,}$$

**A priori form** (order $p = 3$, $C = 5.2$): with $D = C^{1/(p-1)} = \sqrt{5.2} \approx 2.28$, the relation $D|e_{n+1}| \le (D|e_n|)^3$ telescopes to

$$|e_n| \le \frac{1}{D}\big(D|e_0|\big)^{3^n}.$$

With $x_0 = 1.1$, $|e_0| = 0.1$: $D|e_0| \approx 0.228 < 1$, so convergence is guaranteed.

**Numerical check** ($x_0 = 1.1$): $x_1 = g(1.1) = 1.0043$, so $e_1 \approx 4.3 \times 10^{-3}$, matching the asymptotic prediction $4 e_0^3 = 4(0.1)^3 = 4 \times 10^{-3}$; the next error is $\approx 4 e_1^3 \approx 3 \times 10^{-7}$ — the error roughly cubes each step.

## Key takeaway

- A polynomial root of $p$ becomes a fixed point of $g$ whenever $p(x) = g(x) - x$ — recognize this rewriting.
- Convergence near $\alpha$ is decided by $|g'(\alpha)|$; here it is $0$, so it converges for $x_0$ close enough (and on an explicit invariant interval where $\max|g'| < 1$).
- Order = index of the first non-vanishing derivative of $g$ at $\alpha$. Here $g'(\alpha) = g''(\alpha) = 0$, $g'''(\alpha) \neq 0$ ⇒ **order 3**, constant $\tfrac{g'''(\alpha)}{3!} = 4$.
- Error bound from Taylor: $|e_{n+1}| \le \tfrac{M_3}{6}|e_n|^3$ with $M_3 = \max_I |g'''|$.
