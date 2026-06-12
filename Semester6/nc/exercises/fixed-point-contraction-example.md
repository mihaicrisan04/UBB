# Fixed-Point Iteration via Contraction — Worked Example

**Problem.** Show that the equation $x = \dfrac{1+\sin x}{2}$ has a unique solution $\alpha$. Find an interval $[a,b]$ containing $\alpha$ for which the iteration $x_{n+1} = \dfrac{1+\sin x_n}{2}$ converges to $\alpha$ for any $x_0 \in [a,b]$. Discuss the convergence, the order of convergence, and give a bound for the error.

The iteration is $x_{n+1} = g(x_n)$ with $g(x) = \dfrac{1+\sin x}{2}$. A solution of the equation is exactly a **fixed point** of $g$.

## 1. Unique solution $\alpha$

Differentiate:

$$g'(x) = \frac{\cos x}{2} \;\Rightarrow\; |g'(x)| = \frac{|\cos x|}{2} \le \frac12 < 1 \quad \text{for all } x.$$

By the mean value theorem, $|g(x) - g(y)| = |g'(\xi)|\,|x - y| \le \tfrac12 |x - y|$, so $g$ is a **contraction** on $\mathbb{R}$ with Lipschitz constant $\lambda = \tfrac12$. The **Banach contraction principle** then gives a **unique** fixed point $\alpha$.

Furthermore, $\sin x \in [-1, 1]$ gives $g(x) = \tfrac{1+\sin x}{2} \in [0, 1]$ for every $x$, so the fixed point satisfies $\alpha \in [0,1]$. (Numerically $\alpha \approx 0.8878$.)

## 2. An interval $[a,b]$ that works

Take $[a, b] = [0, 1]$ and verify the two convergence conditions:

- **$g$ maps $[0,1]$ into itself:** for $x \in [0,1]$, $\sin x \in [0, \sin 1] = [0, 0.841]$, so $g(x) \in [\tfrac12, 0.921] \subseteq [0,1]$. ✓
- **$g$ is a contraction there:** $\lambda = \max_{[0,1]} |g'| = \dfrac{\cos 0}{2} = \dfrac12 < 1$ ($\cos$ decreases on $[0,1]$, so the max of $|g'|$ is at $x = 0$). ✓

Hence $x_{n+1} = g(x_n)$ converges to $\alpha$ for **any** $x_0 \in [0,1]$. (In fact $g$ contracts on all of $\mathbb{R}$ and lands in $[0,1]$, so it converges from any real $x_0$ — but $[0,1]$ is the clean answer the problem asks for.)

## 3. Convergence, order, error bound

**Convergence** — guaranteed by the global fixed-point theorem: $g \in C^1[0,1]$, $g([0,1]) \subseteq [0,1]$, and $\lambda = \tfrac12 < 1$.

**Order of convergence** — check $g'(\alpha)$. Since $\alpha \approx 0.8878 \neq \tfrac{\pi}{2}$, $\cos\alpha \neq 0$, so

$$g'(\alpha) = \frac{\cos\alpha}{2} \approx 0.316 \neq 0.$$

The first derivative is nonzero, so convergence is **order 1 (linear)**, with asymptotic rate

$$\lim_{n\to\infty} \frac{x_{n+1} - \alpha}{x_n - \alpha} = g'(\alpha) = \frac{\cos\alpha}{2}.$$

**Error bound** — from the contraction with $\lambda = \tfrac12$:

$$\boxed{\,|x_n - \alpha| \le \lambda^n |x_0 - \alpha| = \left(\tfrac12\right)^n |x_0 - \alpha| \le \left(\tfrac12\right)^n\,}$$

(the last step since $|x_0 - \alpha| \le 1$ on $[0,1]$). The **a posteriori** form is convenient in practice:

$$|x_n - \alpha| \le \frac{\lambda}{1 - \lambda}\,|x_n - x_{n-1}| = |x_n - x_{n-1}| \qquad (\text{since } \tfrac{\lambda}{1-\lambda} = 1 \text{ for } \lambda = \tfrac12).$$

## Key takeaway

- A solution of $x = g(x)$ is a fixed point of $g$; rewrite the equation that way first.
- **Existence + uniqueness** come from the contraction property $|g'(x)| \le \lambda < 1$ (Banach principle) — here $|g'| = \tfrac{|\cos x|}{2} \le \tfrac12$ globally.
- **A valid interval** $[a,b]$ needs both $g([a,b]) \subseteq [a,b]$ and $\max_{[a,b]}|g'| < 1$. The range $[0,1]$ of $g$ is the natural candidate.
- **Order** = index of the first nonzero derivative of $g$ at $\alpha$. Here $g'(\alpha) \neq 0$ ⇒ linear; the convergence rate is $g'(\alpha)$, the guaranteed bound uses $\lambda = \max|g'|$.
- **Error:** $|x_n - \alpha| \le \lambda^n |x_0 - \alpha|$ (a priori) or $\tfrac{\lambda}{1-\lambda}|x_n - x_{n-1}|$ (a posteriori).
