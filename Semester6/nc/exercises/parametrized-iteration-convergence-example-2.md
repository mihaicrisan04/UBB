# Parametrized Fixed-Point Iteration — Order & Error (Example 2)

Second example of this type (see `parametrized-iteration-convergence-example.md` for the $\alpha=1$ version).

**Problem.** The iteration $x_{n+1} = 2(1+c)x_n - c x_n^2 - 2$ converges to $\alpha = 2$ for $x_0$ sufficiently close. Find the order of convergence and a bound for the error.

The iteration is $x_{n+1} = g(x_n)$ with $g(x) = 2(1+c)x - c x^2 - 2$.

## Fixed point

$$g(2) = 2(1+c)(2) - c(4) - 2 = 4 + 4c - 4c - 2 = 2 \quad\checkmark \qquad (\text{for every } c),$$

so $\alpha = 2$ is a fixed point regardless of $c$.

## Derivatives at $\alpha = 2$

$$g'(x) = 2(1+c) - 2cx \;\Rightarrow\; g'(2) = 2(1+c) - 4c = 2 - 2c = 2(1-c),$$
$$g''(x) = -2c \;\Rightarrow\; g''(2) = -2c.$$

The order depends on $c$ because $g'(2) = 2(1-c)$ vanishes only at $c = 1$.

## Convergence range

Local convergence (Theorem 3.2) needs $|g'(2)| < 1$:

$$|2 - 2c| < 1 \iff |1 - c| < \tfrac12 \iff \boxed{\tfrac12 < c < \tfrac32} \quad (\text{for } x_0 \text{ close to } 2).$$

## Case 1 — $c \neq 1$ (with $\tfrac12 < c < \tfrac32$): linear, order 1

$g'(2) = 2 - 2c \neq 0$, so convergence is **linear (order 1)** with asymptotic rate

$$\lim_{n\to\infty} \frac{e_{n+1}}{e_n} = g'(2) = 2 - 2c, \qquad e_n = x_n - 2.$$

**Error bound:** $|e_{n+1}| \le \lambda |e_n|$ with $\lambda = \max_I |g'|$ (near $|2-2c|$) on an invariant interval $I \ni 2$, hence

$$|e_n| \le \lambda^n |e_0|, \qquad \lambda = |2 - 2c| < 1.$$

## Case 2 — $c = 1$: quadratic, order 2

Now $g'(2) = 0$ and $g''(2) = -2c = -2 \neq 0$, so by Theorem 3.3 the order is **2 (quadratic)** with constant $\dfrac{g''(2)}{2!} = -1$.

This case is exact: with $c = 1$, $g(x) = 4x - x^2 - 2$, and putting $x_n = 2 + e_n$,

$$g(2 + e_n) = 4(2 + e_n) - (2 + e_n)^2 - 2 = 2 - e_n^2 \;\Rightarrow\; e_{n+1} = -e_n^2.$$

**Error bound:** $|e_{n+1}| = |e_n|^2 \;\Rightarrow\; |e_n| = |e_0|^{2^n}$, converging whenever $|e_0| < 1$. (Equivalently $|e_{n+1}| \le \tfrac{M_2}{2}|e_n|^2$ with $M_2 = \max_I |g''| = 2$, giving $|e_{n+1}| \le |e_n|^2$.)

## Summary

| $c$ | $g'(2)$ | order | error bound |
|---|---|---|---|
| $\tfrac12 < c < \tfrac32,\ c \neq 1$ | $2 - 2c \neq 0$ | **1 (linear)** | $\|e_n\| \le \|2-2c\|^n\,\|e_0\|$ |
| $c = 1$ | $0$ | **2 (quadratic)** | $\|e_n\| = \|e_0\|^{2^n}$ |

Since $g''(x) = -2c$ is constant (nonzero for $c \neq 0$), the order never exceeds 2.

## Key takeaway

- Evaluate $g'(\alpha)$ as a function of the parameter: the value making $g'(\alpha) = 0$ is where the order jumps from linear to quadratic.
- Convergence range comes from $|g'(\alpha)| < 1$.
- $g'(\alpha) \neq 0$: linear, rate $|g'(\alpha)|$, bound $|e_n| \le \lambda^n|e_0|$.
- $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: quadratic, constant $\tfrac{g''(\alpha)}{2!}$.
