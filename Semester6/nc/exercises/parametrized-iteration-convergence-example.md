# Parametrized Fixed-Point Iteration — Order & Error Depending on $c$

**Problem.** Consider the iteration $x_{n+1} = 2 - (1+c)x_n + c\,x_n^2$, with fixed point $\alpha = 1$. Find the order of convergence and an error bound.

The iteration is $x_{n+1} = g(x_n)$ with $g(x) = 2 - (1+c)x + c\,x^2$.

## Fixed point

$$g(1) = 2 - (1+c) + c = 1 \quad\checkmark \qquad (\text{for every } c).$$

## Derivatives at $\alpha = 1$

$$g'(x) = -(1+c) + 2cx \;\Rightarrow\; g'(1) = -(1+c) + 2c = c - 1,$$
$$g''(x) = 2c \;\Rightarrow\; g''(1) = 2c.$$

Because $g'(1) = c - 1$ vanishes only at $c = 1$, the **order depends on the parameter $c$.**

## Convergence condition

Local convergence (Theorem 3.2) requires $|g'(1)| < 1$:

$$|c - 1| < 1 \iff \boxed{0 < c < 2} \quad (\text{for } x_0 \text{ close enough to } 1).$$

## Case 1 — $c \neq 1$ (with $0 < c < 2$): linear, order 1

$g'(1) = c - 1 \neq 0$, so convergence is **linear (order 1)** with asymptotic rate

$$\lim_{n\to\infty}\frac{e_{n+1}}{e_n} = g'(1) = c - 1, \qquad e_n = x_n - 1.$$

**Error bound:** $|e_{n+1}| \le \lambda\,|e_n|$ with $\lambda = \max_I |g'|$ (near $|c-1|$) on an invariant interval $I \ni 1$, hence

$$|e_n| \le \lambda^n\,|e_0|.$$

## Case 2 — $c = 1$: quadratic, order 2

Now $g'(1) = 0$ and $g''(1) = 2c = 2 \neq 0$, so by Theorem 3.3 the order is **2 (quadratic)** with constant $\dfrac{g''(1)}{2!} = 1$.

This case is exact: for $c = 1$, $g(x) = 2 - 2x + x^2 = (x-1)^2 + 1$, so

$$e_{n+1} = x_{n+1} - 1 = (x_n - 1)^2 = e_n^2.$$

**Error bound:** $|e_{n+1}| = |e_n|^2$ exactly $\;\Rightarrow\; |e_n| = |e_0|^{2^n}$, converging whenever $|e_0| < 1$.

## Summary

| $c$ | $g'(1)$ | order | error bound |
|---|---|---|---|
| $0 < c < 2,\ c \neq 1$ | $c - 1 \neq 0$ | **1 (linear)** | $\|e_n\| \le \|c-1\|^n\,\|e_0\|$ |
| $c = 1$ | $0$ | **2 (quadratic)** | $\|e_n\| = \|e_0\|^{2^n}$ |

Since $g''(x) = 2c$ is constant (never zero for $c \neq 0$), the order can never exceed 2 — no value of $c$ gives cubic convergence.

## Key takeaway

- For a parametrized iteration, evaluate $g'(\alpha)$ as a function of the parameter: the value of the parameter that makes $g'(\alpha) = 0$ is exactly where the order jumps from linear to quadratic.
- Convergence range comes from $|g'(\alpha)| < 1$.
- When $g'(\alpha) \neq 0$: linear, rate $|g'(\alpha)|$, bound $|e_n| \le \lambda^n|e_0|$.
- When $g'(\alpha) = 0$, $g''(\alpha) \neq 0$: quadratic, constant $\tfrac{g''(\alpha)}{2!}$.
