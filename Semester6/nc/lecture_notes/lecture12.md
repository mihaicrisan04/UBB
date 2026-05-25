# Lecture 12 — Higher-Order Iteration, Multiple Roots, and Newton's Method for Systems

## Overview
This lecture extends the one-point iteration theory: it characterizes the order of convergence of $x_{n+1} = g(x_n)$ through the vanishing of derivatives of $g$ at the fixed point, then studies the difficulties of approximating multiple roots (uncertainty and loss of precision in Newton's method) and how to restore quadratic convergence. It closes by generalizing Newton's method to systems of nonlinear equations using the Jacobian matrix.

## Key Notions & Definitions

- **Order of convergence via $g$'s derivatives** — the speed of $x_{n+1} = g(x_n)$ is governed by how many derivatives of $g$ vanish at $\alpha$. If $g'(\alpha) \neq 0$ convergence is linear; higher order requires $g'(\alpha) = \dots = g^{(p-1)}(\alpha) = 0$, $g^{(p)}(\alpha) \neq 0$.

- **Root of multiplicity $m$** — $\alpha$ is a root of $f$ of multiplicity $m > 1$ if $$f(x) = (x - \alpha)^m h(x), \quad h \text{ continuous at } \alpha, \ h(\alpha) \neq 0.$$ For smooth $h$ this is equivalent to $$f(\alpha) = f'(\alpha) = \dots = f^{(m-1)}(\alpha) = 0, \quad f^{(m)}(\alpha) \neq 0.$$

- **Interval of uncertainty** — the unavoidable range around a computed root caused by measuring/rounding/truncation errors; it widens for multiple roots, limiting attainable accuracy.

- **Modified Newton's method (function modification)** — solving $u(x) = \frac{f(x)}{f'(x)} = 0$ instead of $f(x) = 0$, since $\alpha$ is a simple root of $u$; gives $$x_{n+1} = x_n - \frac{f(x_n) f'(x_n)}{\big(f'(x_n)\big)^2 - f(x_n) f''(x_n)}.$$

- **Newton's method for multiple roots (method modification)** — if the multiplicity $m$ is known, $$x_{n+1} = x_n - m\,\frac{f(x_n)}{f'(x_n)},$$ which restores quadratic convergence.

- **Jacobian matrix** — for $f = (f_1, f_2)^T$ of $x = (x_1, x_2)^T$, $$J_f(x) = \begin{bmatrix} \dfrac{\partial f_1}{\partial x_1} & \dfrac{\partial f_1}{\partial x_2} \\[2mm] \dfrac{\partial f_2}{\partial x_1} & \dfrac{\partial f_2}{\partial x_2} \end{bmatrix};$$ it plays the role of the derivative in the multidimensional setting.

- **Newton's method for nonlinear systems** — $$x_{n+1} = x_n - \big(J_f(x_n)\big)^{-1} f(x_n), \quad n \geq 0.$$

## Theorems & Results

### Theorem 3.1 (recap: $C^1$ global fixed-point theorem)
For $g \in C^1[a,b]$ with $g([a,b]) \subseteq [a,b]$ and $\lambda = \max_{[a,b]}|g'| < 1$: $g$ has a unique fixed point $\alpha$; $x_{n+1} = g(x_n)$ converges for any $x_0 \in [a,b]$; $|x_n - \alpha| \leq \lambda^n|x_0 - \alpha| \leq \frac{\lambda^n}{1-\lambda}|x_1 - x_0|$; and $\lim_{n\to\infty}\frac{x_{n+1}-\alpha}{x_n-\alpha} = g'(\alpha)$ (linear, rate $\leq \lambda$, if $g'(\alpha) \neq 0$).

### Theorem 3.2 (recap: local version)
Same conclusions hold if $\alpha$ is a fixed point, $g$ is $C^1$ near $\alpha$, and $|g'(\alpha)| < 1$, provided $x_0$ is chosen close enough to $\alpha$.

### Theorem 3.3 (higher-order convergence of fixed-point iteration)
Assume $\alpha$ is a fixed point of $g$, $g$ is $p$ times continuously differentiable near $\alpha$ ($p \geq 2$), and $$g'(\alpha) = \dots = g^{(p-1)}(\alpha) = 0, \quad g^{(p)}(\alpha) \neq 0.$$ Then, for $x_0$ sufficiently close to $\alpha$, $x_{n+1} = g(x_n)$ converges to $\alpha$ with order $p$ and $$\lim_{n \to \infty} \frac{x_{n+1} - \alpha}{(x_n - \alpha)^p} = \frac{1}{p!}\,g^{(p)}(\alpha).$$
Proof sketch: convergence follows from Theorem 3.2 (since $g'(\alpha) = 0$). Taylor-expand $g(x_n)$ around $\alpha$; using $g(\alpha) = \alpha$ and the vanishing derivatives, all middle terms drop out, leaving $x_{n+1} - \alpha = \frac{(x_n - \alpha)^p}{p!} g^{(p)}(\xi_n)$ for some $\xi_n$ between $x_n$ and $\alpha$. Dividing and letting $n \to \infty$ (so $\xi_n \to \alpha$) gives the limit.

### Loss of precision in Newton's method at multiple roots
For $f(x) = (x-\alpha)^m h(x)$ with $m > 1$, writing Newton as $g(x) = x - (x-\alpha)\varphi(x)$ where $\varphi(x) = \frac{h(x)}{m\,h(x) + (x-\alpha)h'(x)}$ and $\varphi(\alpha) = \frac{1}{m}$ gives $$g'(\alpha) = 1 - \varphi(\alpha) = 1 - \frac{1}{m} \neq 0.$$ Hence ordinary Newton converges only **linearly** at a multiple root, with rate $1 - \frac{1}{m}$. The function-modification fix (solving $u = f/f'$) restores $p = 2$ but needs $f''$, costs more, and its denominator can become tiny as $x_n \to \alpha$. The method-modification fix $x_{n+1} = x_n - m\frac{f(x_n)}{f'(x_n)}$ gives $g'(\alpha) = 1 - m\varphi(\alpha) = 0$, so $p = 2$ is recovered with $\lim \frac{x_{n+1}-\alpha}{(x_n-\alpha)^2} = \frac{1}{2}g''(\alpha)$.

### Newton's method for systems (Section 5)
The one-point theory carries over with the norm replacing absolute value and the Jacobian replacing the derivative. Derived from Taylor-expanding each $f_i$, the iteration is $x_{n+1} = x_n - (J_f(x_n))^{-1} f(x_n)$. In practice one does not invert $J_f$; instead one solves the linear system for the correction $\delta_{n+1}$: $$J_f(x_n)\,\delta_{n+1} = -f(x_n), \qquad x_{n+1} = x_n + \delta_{n+1},$$ which costs about one-third of the operations of inversion.

## Example Problems

### Example: Newton's method analyzed by Theorem 3.3 (Example 3.4)
For a simple root ($f(\alpha)=0$, $f'(\alpha)\neq 0$), $g(x) = x - \frac{f(x)}{f'(x)}$ gives $g'(x) = \frac{f(x)f''(x)}{(f'(x))^2}$, so $g'(\alpha) = 0$ and $g''(\alpha) = \frac{f''(\alpha)}{f'(\alpha)}$. By Theorem 3.3 with $p = 2$, $$\lim_{n\to\infty}\frac{x_{n+1}-\alpha}{(x_n-\alpha)^2} = \tfrac12 g''(\alpha) = \frac{f''(\alpha)}{2f'(\alpha)},$$ recovering Newton's quadratic convergence — a direct use of Theorem 3.3.

### Example: $x^2 - 3 = 0$ revisited via Newton (Example 3.5)
With $f(x) = x^2 - 3$, $f'(x) = 2x$, Newton gives $g(x) = \frac{1}{2}\big(x + \frac{3}{x}\big)$. Then $g'(\alpha) = \frac{1}{2}(1 - 3/\alpha^2) = 0$ and $g''(x) = \frac{3}{x^3}$, so $g''(\sqrt3) = \frac{1}{\sqrt3} \neq 0$. By Theorem 3.3 the iteration converges quadratically ($p = 2$) to $\alpha = \sqrt3$. Note this is exactly reformulation (c) from Lecture 11 — its good convergence was not luck, it is Newton's method.

### Example: Convergence of $x_{n+1} = \frac{c}{2}x_n^2 + (c+3)x_n + 4$ to $\alpha = -2$ (Example 3.6)
With $g(x) = \frac{c}{2}x^2 + (c+3)x + 4$: $g(-2) = -2$ (so $\alpha = -2$ is fixed), $g'(-2) = 3 - c$, $g''(x) = c$. Convergence needs $|g'(\alpha)| < 1 \iff |3-c| < 1 \iff c \in (2,4)$ — then linear with rate $|c-3|$ (Theorem 3.2). For $c = 3$ we get $g'(\alpha) = 0$, so by Theorem 3.3 convergence is quadratic with $\lim \frac{x_{n+1}+2}{(x_n+2)^2} = \tfrac12 g''(-2) = \frac{c}{2} = \frac{3}{2}$.

### Example: Order-4 convergence of $x_{n+1} = x_n^5 - 10x_n^3 - 20x_n^2 - 15x_n - 5$ to $\alpha = -1$ (Example 3.7)
$g(-1) = -1$, so $\alpha = -1$ is a fixed point. Successively: $g'(-1) = 0$, $g''(-1) = 0$, $g'''(-1) = 0$, $g^{(4)}(-1) = -120 \neq 0$. By Theorem 3.3 with $p = 4$, for $x_0$ close to $-1$ (specifically $|x_0 + 1| < 1$), $$\lim_{n\to\infty}\frac{x_{n+1}+1}{(x_n+1)^4} = \frac{1}{4!}g^{(4)}(-1) = -5,$$ giving order-4 convergence with error $|x_{n+1}+1| \leq c\,|x_0+1|^{4^{n+1}}$. A clean illustration of Theorem 3.3 for $p > 2$.

### Example: Uncertainty at a double root (Example 4.2)
Compare $f_1(x) = x^2 - 3$ (simple root $\sqrt3$) with $f_2(x) = x^2(x^2 - 6) + 9$ (double root $\sqrt3$, since $f_2'(x) = 4x(x^2-3)$). In four-digit arithmetic, $f_1$ pins $\alpha \in (1.731, 1.733)$, whereas $f_2 = 0$ for all $1.726 \leq x \leq 1.738$, so $\alpha \in [1.726, 1.738]$ — a far wider interval of uncertainty. This illustrates why multiple roots limit attainable accuracy (the multiplicity definition 4.1).

### Example: Newton's method for a $2\times2$ system (Example 5.1)
Solve $f_1 = 4x_1^2 + x_2^2 - 4 = 0$, $f_2 = x_1 + x_2 - \sin(x_1 - x_2) = 0$ (an ellipse meeting a curve; two roots near $(\pm1, 0)$). Using $x_0 = [1, 0]^T$ and the Jacobian $$J_f = \begin{bmatrix} 8x_1 & 2x_2 \\ 1 - \cos(x_1 - x_2) & 1 + \cos(x_1 - x_2) \end{bmatrix},$$ solving $J_f \delta = -f$ at each step (rather than inverting) converges in 3 iterations to $x \approx (0.9986069441, -0.1055304923)$ with residuals dropping to $\sim 10^{-11}$. This demonstrates the system form $x_{n+1} = x_n - (J_f(x_n))^{-1}f(x_n)$ and the quadratic-like fast convergence carried over from the scalar case.

## Cheat-sheet
- Higher-order fixed point: if $g'(\alpha) = \dots = g^{(p-1)}(\alpha) = 0$ and $g^{(p)}(\alpha) \neq 0$, then order $p$ with $\lim \frac{x_{n+1}-\alpha}{(x_n-\alpha)^p} = \frac{g^{(p)}(\alpha)}{p!}$.
- Multiplicity $m$: $f = (x-\alpha)^m h$, $h(\alpha)\neq 0$ $\iff$ $f(\alpha) = \dots = f^{(m-1)}(\alpha) = 0$, $f^{(m)}(\alpha) \neq 0$.
- Plain Newton at a root of multiplicity $m$: only linear, $g'(\alpha) = 1 - \frac{1}{m}$.
- Restore quadratic: either solve $u = f/f' = 0$, i.e. $x_{n+1} = x_n - \frac{f f'}{(f')^2 - f f''}$, or use $x_{n+1} = x_n - m\frac{f}{f'}$ (known $m$).
- Newton's quadratic constant (simple root): $\lim \frac{x_{n+1}-\alpha}{(x_n-\alpha)^2} = \frac{f''(\alpha)}{2f'(\alpha)}$.
- Systems: $x_{n+1} = x_n - (J_f(x_n))^{-1}f(x_n)$; in practice solve $J_f(x_n)\delta_{n+1} = -f(x_n)$, then $x_{n+1} = x_n + \delta_{n+1}$ (~3x cheaper than inverting).
- Multiple roots also suffer a wider interval of uncertainty, capping accuracy.
