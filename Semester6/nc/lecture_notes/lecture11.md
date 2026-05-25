# Lecture 11 — Numerical Solution of Nonlinear Equations: Rootfinding Methods and Fixed-Point Iteration

## Overview
This lecture opens Chapter 5 on solving $f(x) = 0$ numerically. It introduces iterative methods and the notion of order of convergence, then covers three classical rootfinding methods (bisection, secant, Newton) with their convergence analysis, and finally develops the general theory of one-point (fixed-point) iteration via the Banach contraction principle.

## Key Notions & Definitions

- **Root / zero of $f$** — a number $\alpha \in \mathbb{C}$ satisfying $f(\alpha) = 0$. The general problem $f(x) = 0$ rarely admits closed-form solutions, so roots are approximated to a chosen accuracy.

- **Iterative method** — a procedure that uses one or more initial guesses $x_0, x_1, \dots$ to produce a sequence $\{x_n\}_{n \in \mathbb{N}}$ with $\lim_{n \to \infty} x_n = \alpha$. Analyzing such a method means proving convergence, finding the interval of convergence (set of valid initial guesses), and determining the speed of convergence.

- **Order of convergence** — a sequence $\{x_n\}$ converges to $\alpha$ with order $p \geq 1$ if there is a constant $c > 0$ independent of $n$ with $$|x_{n+1} - \alpha| \leq c\,|x_n - \alpha|^p, \quad \forall n \in \mathbb{N}.$$ If $p = 1$ the convergence is **linear** (requires $c < 1$, and $c$ is the **rate of linear convergence**); if $1 < p < 2$ it is **superlinear**.

- **Bisection method** — a two-point method that repeatedly halves an interval $[a,b]$ with $f(a)f(b) < 0$, keeping the half where the sign changes. The approximation at step $n$ is the midpoint $$c_n = \frac{a_n + b_n}{2}.$$

- **Secant method** — approximates $f$ by the secant line through $(x_0, f(x_0))$ and $(x_1, f(x_1))$ and takes the $x$-intercept; recursively $$x_{n+1} = x_n - f(x_n)\,\frac{x_n - x_{n-1}}{f(x_n) - f(x_{n-1})}, \quad n = 1, 2, \dots$$ It is a two-point method, can find complex roots, and does not always converge.

- **Newton's (tangent) method** — a one-point method approximating $f$ by its tangent at $x_n$ and taking the $x$-intercept: $$x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}, \quad n = 0, 1, \dots$$

- **Fixed-point reformulation** — rewriting $f(x) = 0$ as $x = g(x)$ and seeking a fixed point of $g$. This form is not restrictive: any equation can be rewritten this way in many ways.

- **Contraction** — $g \in C[a,b]$ is a contraction if there is $0 < \lambda < 1$ with $$|g(x) - g(y)| \leq \lambda\,|x - y|, \quad \forall x, y \in [a,b].$$ If $g$ is $C^1$, then $\lambda = \max_{x \in [a,b]} |g'(x)|$ works (via the Mean Value Theorem).

## Theorems & Results

### Theorem 2.1 (Intermediate Value Theorem)
If $f : [a,b] \to \mathbb{R}$ is continuous, it attains every value between $f(a)$ and $f(b)$. Consequence: if a continuous $f$ has opposite signs at the endpoints of $[a,b]$, it has at least one root inside.

### Theorem 2.2 (Rolle's Theorem)
If $f$ is continuous on $[a,b]$, differentiable on $(a,b)$, and $f(a) = f(b)$, then there exists $c \in (a,b)$ with $f'(c) = 0$. Consequence: between any two distinct real roots of $f$ lies a root of $f'$. Combining IVT and Rolle lets us count and locate real zeros by tracking sign changes at roots of $f'$ and at domain endpoints.

### Bisection convergence
Under $f(a)f(b) < 0$, the embedded intervals satisfy $\alpha \in [a_n, b_n]$, with $\{a_n\}$ increasing, $\{b_n\}$ decreasing, and $a_n, b_n, c_n \to \alpha$. The error obeys $$|x_n - \alpha| \leq b_n - a_n = \frac{b - a}{2^n}, \qquad |x_{n+1} - \alpha| \leq \tfrac{1}{2}|x_n - \alpha|,$$ so bisection converges linearly ($p = 1$) with rate $\tfrac{1}{2}$. The number of iterations for tolerance $\varepsilon$ is found from $\frac{b-a}{2^n} \leq \varepsilon$, i.e. $n \geq \log_2\!\big(\frac{b-a}{\varepsilon}\big)$. It is guaranteed to converge (continuity + sign change) but is slow and only finds real roots.

### Theorem 2.7 (Secant method order of convergence)
Assume $f, f', f''$ are continuous on $I_\varepsilon = (\alpha - \varepsilon, \alpha + \varepsilon)$ containing the simple root $\alpha$ ($f'(\alpha) \neq 0$). For $x_0, x_1$ sufficiently close to $\alpha$, the iterates converge to $\alpha$ with order $$p = r = \frac{1 + \sqrt 5}{2} \approx 1.618033\dots$$ the **golden ratio**; thus the secant method converges superlinearly. The analysis uses divided differences to derive $$x_{n+1} - \alpha = (x_n - \alpha)(x_{n-1} - \alpha)\,\frac{f''(\xi_n)}{2 f'(\zeta_n)}.$$ "Sufficiently close" is quantified by $M_\varepsilon = \frac{\max_{I_\varepsilon}|f''|}{2 \min_{I_\varepsilon}|f'|}$ with $\max\{M_\varepsilon e_0, M_\varepsilon e_1\} < 1$, where $e_0 = |x_0 - \alpha|$, $e_1 = |x_1 - \alpha|$.

### Theorem 2.10 (Newton's method order of convergence)
Assume $f, f', f''$ continuous on $I_\varepsilon$ containing the simple root $\alpha$ ($f'(\alpha) \neq 0$). If $x_0$ is sufficiently close to $\alpha$, the iterates converge to $\alpha$ with $$\lim_{n \to \infty} \frac{x_{n+1} - \alpha}{(x_n - \alpha)^2} = \frac{f''(\alpha)}{2 f'(\alpha)},$$ so Newton's method has order $p = 2$ (quadratic). The error satisfies $x_{n+1} - \alpha = (x_n - \alpha)^2 \frac{f''(\xi_n)}{2 f'(x_n)}$ with $\xi_n$ between $\alpha$ and $x_n$. Newton needs two evaluations per step ($f$ and $f'$), is one-step, and can find complex roots.

### Comparison Newton vs Secant (Section 2.4)
If $f'(x_n) \approx \frac{f(x_n) - f(x_{n-1})}{x_n - x_{n-1}}$ is plugged into Newton, one recovers the secant formula. Newton needs two evaluations per iterate, the secant only one (the older value is reused). With $t$ = cost of $f$ and $s\cdot t$ = cost of $f'$, the minimum times are $T_N = \frac{(1+s) t K}{\log 2}$ and $T_S = \frac{t K}{\log r}$, giving $\frac{T_S}{T_N} = \frac{\log 2}{(1+s)\log r}$. The secant method is more efficient when $$s > \frac{\log 2}{\log r} - 1 \approx 0.44,$$ i.e. when evaluating $f'$ costs more than about 44% of evaluating $f$.

### Lemma 3.2 (existence of a fixed point)
If $g \in C[a,b]$ and $g([a,b]) \subseteq [a,b]$, then $g$ has at least one fixed point in $[a,b]$. Proof sketch: apply IVT to $G(x) = g(x) - x$, which satisfies $G(a) \geq 0$ and $G(b) \leq 0$, so $G$ has a zero.

### Theorem 3.3 (Banach contraction principle)
Let $g \in C[a,b]$ with $g([a,b]) \subseteq [a,b]$ and $g$ a contraction with constant $0 < \lambda < 1$. Then $g$ has a **unique** fixed point $\alpha \in [a,b]$, and for any $x_0 \in [a,b]$ the iterates $x_{n+1} = g(x_n)$ converge to $\alpha$ with $$|x_n - \alpha| \leq \lambda\,|x_{n-1} - \alpha|, \qquad |x_n - \alpha| \leq \frac{\lambda^n}{1 - \lambda}|x_1 - x_0|.$$
Proof sketch: existence from Lemma 3.2. Uniqueness: two fixed points give $|\alpha - \beta| = |g(\alpha) - g(\beta)| \leq \lambda|\alpha - \beta|$, forcing $(1-\lambda)|\alpha-\beta| \leq 0$, so $\alpha = \beta$. Convergence: iterating the contraction bound gives $|x_n - \alpha| \leq \lambda^n |x_0 - \alpha| \to 0$. The second bound comes from $|x_0 - \alpha| \leq \frac{1}{1-\lambda}|x_1 - x_0|$. A useful stopping criterion is $|x_{n+1} - x_n| \leq \frac{1-\lambda}{\lambda}\varepsilon$.

### Theorem 3.5 (differentiable version, global)
Let $g \in C^1[a,b]$ with $g([a,b]) \subseteq [a,b]$ and $\lambda := \max_{x \in [a,b]} |g'(x)| < 1$. Then $g$ has a unique fixed point $\alpha \in [a,b]$; for any $x_0 \in [a,b]$, $x_{n+1} = g(x_n) \to \alpha$ with $|x_n - \alpha| \leq \lambda^n|x_0 - \alpha| \leq \frac{\lambda^n}{1-\lambda}|x_1 - x_0|$; and $$\lim_{n \to \infty} \frac{x_{n+1} - \alpha}{x_n - \alpha} = g'(\alpha),$$ so if $g'(\alpha) \neq 0$ the convergence is linear with rate bounded by $\lambda$.

### Theorem 3.6 (local version)
If $\alpha$ is a fixed point of $g$, $g$ is $C^1$ near $\alpha$, and $|g'(\alpha)| < 1$, then the conclusions of Theorem 3.5 still hold provided $x_0$ is chosen sufficiently close to $\alpha$.

## Example Problems

### Example: Largest root of $x^6 - x - 1 = 0$ by bisection (Example 2.4)
Goal: largest root with error $\varepsilon = 0.001$. Since $f'(x) = 6x^5 - 1$ has a single real zero at $\frac{1}{\sqrt[5]{6}}$ and $f\big(\frac{1}{\sqrt[5]{6}}\big) < 0$, the variation table ($+,-,+$) shows two real roots. From $f(1) = -1$, $f(2) = 61$, the sought root lies in $(1,2)$, so start with $[a_1, b_1] = [1,2]$. The iteration count comes from the bisection error bound (above): $n \geq \log_2(1/10^{-3}) \approx 9.97$, so $n = 10$ iterations give the desired precision, yielding $\alpha \approx 1.1338$. This directly applies the bisection convergence/error estimate.

### Example: Same equation by the secant method (Example 2.6)
Starting from $x_0 = 2$, $x_1 = 1$, the secant iteration reaches $x_8 = 1.13472414$, equal to $\alpha$ to nine significant digits. The fast, irregular error decrease illustrates Theorem 2.7 (superlinear, golden-ratio order $\approx 1.618$) — far fewer iterations than the 10 needed by bisection.

### Example: Same equation by Newton's method (Example 2.9)
With $f(x) = x^6 - x - 1$, $$x_{n+1} = x_n - \frac{x_n^6 - x_n - 1}{6x_n^5 - 1}.$$ Starting from $x_0 = 1.5$, the iterates reach $x_6 = 1.13472414$ accurate to machine precision. The roughly doubling number of correct digits per step illustrates Theorem 2.10 (quadratic, $p = 2$).

### Example: Four fixed-point reformulations of $x^2 - 3 = 0$ (Examples 3.1 and 3.7)
With $\alpha = \sqrt 3$, the equation is rewritten as (a) $x = x^2 + x - 3$, (b) $x = 3/x$, (c) $x = \frac{1}{2}(x + 3/x)$, (d) $x = x + c(x^2 - 3)$. Applying Theorem 3.6 (the local $|g'(\alpha)| < 1$ test):
- (a) $g'(\alpha) = 2\sqrt 3 + 1 > 1$ — does not converge.
- (b) $g'(\alpha) = -1$ — does not converge.
- (c) $g'(\alpha) = \frac{1}{2}(1 - 3/\alpha^2) = 0$ — converges (at least linearly; in fact this is Newton's method, so quadratically).
- (d) $g'(\alpha) = 1 + 2c\sqrt 3$; choose $-\frac{1}{\sqrt 3} < c < 0$ for convergence, e.g. $c = -\frac{1}{4}$ gives a good rate.
This shows the fixed-point form is highly non-unique and that the choice determines convergence.

### Example: Real roots of $x - 1 - \arctan x = 0$ and convergence of $x_{n+1} = 1 + \arctan x_n$ (Example 3.8)
Let $f(x) = x - 1 - \arctan x$. Then $f'(x) = \frac{x^2}{1+x^2} \geq 0$ (zero only at $0$), so the variation table is $-,-,+$ and there is exactly one real root $\alpha > 0$; checking values gives $\alpha \in \big(1, 1 + \frac{\pi}{2}\big)$. To analyze $g(x) = 1 + \arctan x$ on $[1, 1 + \frac{\pi}{2}]$: since $g'(x) = \frac{1}{1+x^2} > 0$, $g$ is increasing and maps the interval into itself ($g(1) = 1 + \frac{\pi}{4} > 1$, $g(1+\frac{\pi}{2}) < 1 + \frac{\pi}{2}$). Because $g'$ is decreasing there, $g'(x) \leq g'(1) = \tfrac{1}{2} < 1$, so by Theorem 3.5 with $\lambda = \tfrac{1}{2}$ the iteration converges for any $x_0 \in [1, 1+\frac{\pi}{2}]$ with error $|x_n - \alpha| \leq \frac{1}{2^{n-1}}|x_1 - x_0|$. The exact root is $\alpha = 2.1322679602$, reached quickly in the table. This is a direct application of the global Theorem 3.5.

## Cheat-sheet
- Order of convergence: $|x_{n+1} - \alpha| \leq c|x_n - \alpha|^p$; $p=1$ linear (need $c<1$), $1<p<2$ superlinear.
- Bisection: $c_n = \frac{a_n+b_n}{2}$; error $\leq \frac{b-a}{2^n}$; linear, rate $\tfrac12$; iterations $n \geq \log_2\!\frac{b-a}{\varepsilon}$; needs $f(a)f(b)<0$.
- Secant: $x_{n+1} = x_n - f(x_n)\frac{x_n - x_{n-1}}{f(x_n) - f(x_{n-1})}$; order $p = \frac{1+\sqrt5}{2} \approx 1.618$.
- Newton: $x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}$; order $p = 2$; $\lim \frac{x_{n+1}-\alpha}{(x_n-\alpha)^2} = \frac{f''(\alpha)}{2f'(\alpha)}$.
- Secant beats Newton in time when $s > \frac{\log 2}{\log r} - 1 \approx 0.44$.
- Fixed-point: rewrite $f(x)=0$ as $x=g(x)$; converges if $g$ maps $[a,b]$ into itself and $\lambda = \max|g'| < 1$ (global) or $|g'(\alpha)| < 1$ (local).
- Banach/error bounds: $|x_n - \alpha| \leq \lambda^n|x_0-\alpha| \leq \frac{\lambda^n}{1-\lambda}|x_1-x_0|$; stopping: $|x_{n+1}-x_n| \leq \frac{1-\lambda}{\lambda}\varepsilon$.
- IVT locates roots via sign change; Rolle: a root of $f'$ lies between two roots of $f$.
