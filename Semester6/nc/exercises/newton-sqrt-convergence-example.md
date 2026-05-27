# One-Point Iterative Method for $\sqrt{5}$ — Order and Error Bound

**Problem.** Define a one-point iterative method to approximate $\sqrt{5}$, for a sufficiently close initial value. Find the order of convergence and a bound for the error of the approximation.

## 1. Set up as a root-finding problem

$\sqrt{5}$ is the positive root of

$$f(x) = x^2 - 5 = 0, \qquad f'(x) = 2x, \qquad f''(x) = 2.$$

## 2. Define the one-point method (Newton)

$$x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)} = x_n - \frac{x_n^2 - 5}{2 x_n} = \frac{1}{2}\left(x_n + \frac{5}{x_n}\right) =: g(x_n).$$

So the iteration function is $g(x) = \dfrac{1}{2}\left(x + \dfrac{5}{x}\right)$ (the Babylonian / Heron square-root iteration). Take a sufficiently close start, e.g. $x_0 = 2$.

**Fixed-point check:**
$$g(\sqrt{5}) = \frac{1}{2}\left(\sqrt{5} + \frac{5}{\sqrt{5}}\right) = \frac{1}{2}\left(\sqrt{5} + \sqrt{5}\right) = \sqrt{5}, \quad\checkmark$$
so $\alpha = \sqrt{5}$ is a fixed point of $g$.

## 3. Order of convergence (Theorem 3.3)

Differentiate the iteration function:

$$g'(x) = \frac{1}{2}\left(1 - \frac{5}{x^2}\right), \qquad g''(x) = \frac{5}{x^3}.$$

Evaluate at $\alpha = \sqrt{5}$ (so $\alpha^2 = 5$, $\alpha^3 = 5\sqrt{5}$):

$$g'(\alpha) = \frac{1}{2}\left(1 - \frac{5}{5}\right) = 0, \qquad g''(\alpha) = \frac{5}{5\sqrt{5}} = \frac{1}{\sqrt{5}} \neq 0.$$

Since $g'(\alpha) = 0$ but $g''(\alpha) \neq 0$, **Theorem 3.3** with $p = 2$ gives convergence of **order 2** (quadratic) for $x_0$ sufficiently close to $\alpha$, with asymptotic error constant

$$\lim_{n \to \infty} \frac{x_{n+1} - \alpha}{(x_n - \alpha)^2} = \frac{g''(\alpha)}{2!} = \frac{1}{2\sqrt{5}} \approx 0.2236.$$

This matches the general simple-root Newton constant $\dfrac{f''(\alpha)}{2 f'(\alpha)} = \dfrac{2}{2 \cdot 2\sqrt{5}} = \dfrac{1}{2\sqrt{5}}$.

## 4. Error bound

Let $e_n = x_n - \alpha$. Taylor-expanding $g$ about $\alpha$, using $g(\alpha) = \alpha$ and $g'(\alpha) = 0$:

$$e_{n+1} = \frac{g''(\xi_n)}{2}\, e_n^2, \quad \xi_n \text{ between } x_n \text{ and } \alpha \;\Rightarrow\; |e_{n+1}| \le \frac{M_2}{2}\,|e_n|^2,$$

where $M_2 = \max_I |g''|$ on an interval $I$ around the root.

Work on $I = [2, 3]$, which contains $\sqrt{5} \approx 2.236$ and satisfies $g(I) \subseteq I$ (so the iteration stays in $I$ — check: $g$ has its minimum $\sqrt5$ at the root and $g(2) = 2.25$, $g(3) \approx 2.333$). Using the Newton form $\frac{M}{2m}$ with $M = \max_I |f''| = 2$ and $m = \min_I |f'| = 2 \cdot 2 = 4$:

$$\boxed{\,|e_{n+1}| \le \frac{M}{2m}\,|e_n|^2 = \frac{2}{2 \cdot 4}\,|e_n|^2 = \frac{1}{4}\,|e_n|^2\,}$$

**A priori bound.** With $C = \tfrac14$, the relation $C|e_{n+1}| \le (C|e_n|)^2$ telescopes to

$$|e_n| \le \frac{1}{C}\big(C|e_0|\big)^{2^n} = 4\left(\frac{|e_0|}{4}\right)^{2^n}.$$

With $x_0 = 2$: $|e_0| = |2 - \sqrt{5}| \approx 0.236$, so $C|e_0| \approx 0.059 < 1$ — convergence is guaranteed and

$$|e_n| \le 4\,(0.059)^{2^n}.$$

## 5. Numerical check ($x_0 = 2$)

$$x_1 = 2.25, \quad x_2 = 2.236111, \quad x_3 = 2.2360680, \qquad \sqrt{5} = 2.2360680\ldots$$

Errors $\approx 1.4\times 10^{-2},\ 4.3\times 10^{-5},\ 4\times 10^{-10}$: the error roughly **squares** each step (times $\tfrac{1}{2\sqrt5}$), confirming order 2. The bound check: $|e_2| \le \tfrac14 |e_1|^2 = \tfrac14 (0.0139)^2 \approx 4.8\times 10^{-5}$, consistent with the actual $4.3\times 10^{-5}$.

## Key takeaway

To build a one-point method for $\sqrt{a}$:
1. Write it as $f(x) = x^2 - a = 0$ and apply Newton → $g(x) = \tfrac12\left(x + \tfrac{a}{x}\right)$.
2. Confirm $\alpha = \sqrt{a}$ is a fixed point and compute $g'(\alpha), g''(\alpha)$. Here $g'(\alpha) = 0$, $g''(\alpha) \neq 0$ ⇒ **order 2** (Theorem 3.3).
3. Error bound from Taylor: $|e_{n+1}| \le \tfrac{M}{2m}|e_n|^2$ with $M = \max|f''|$, $m = \min|f'|$ on an interval around the root; this gives the a priori bound $|e_n| \le \tfrac1C (C|e_0|)^{2^n}$, $C = \tfrac{M}{2m}$.
