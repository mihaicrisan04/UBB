# Lecture 8 — Cubic Splines and Least Squares Approximation

## Overview
This lecture develops **cubic spline interpolation** ($\mathbb{S}_3^1$ via slopes and $\mathbb{S}_3^2$ via second derivatives/moments), covering the standard boundary-condition variants (complete, natural, endpoint-second-derivative, not-a-knot/deBoor) and the resulting tridiagonal systems, plus minimality (smoothness) properties. It then introduces **least squares (mean square) approximation**: scalar products with weights, the normal equations, and **orthogonal polynomials** as a numerically superior basis.

## Key Notions & Definitions

- **Cubic spline space** — $\mathbb{S}_m^k(\Delta)$ with $m = 3$. Cubic splines are the most widely used: smooth and convenient, applied in modeling, aircraft/shipbuilding surfaces, computer graphics.

- **Cubic spline via slopes ($s_3 \in \mathbb{S}_3^1(\Delta)$)** — Continuity of the first derivative is imposed by prescribing slopes $m_1, \dots, m_n$ at the nodes. One seeks $s_3(f;\cdot)$ with $s_3|_{[x_i,x_{i+1}]} = p_i \in \mathbb{P}_3$, $s_3(f;x_i) = f_i$, $s_3'(f;x_i) = m_i$. On each subinterval $p_i$ is the unique solution of the Hermite problem $p_i(x_i)=f_i$, $p_i(x_{i+1})=f_{i+1}$, $p_i'(x_i)=m_i$, $p_i'(x_{i+1})=m_{i+1}$.

- **Taylor (local) form of the piece** — $$p_i(x) = c_{i,0} + c_{i,1}(x-x_i) + c_{i,2}(x-x_i)^2 + c_{i,3}(x-x_i)^3,$$ with $h_i = x_{i+1}-x_i$ and $$c_{i,0} = f_i,\quad c_{i,1} = m_i,\quad c_{i,2} = \frac{3f[x_i,x_{i+1}] - 2m_i - m_{i+1}}{h_i},\quad c_{i,3} = \frac{m_{i+1} - 2f[x_i,x_{i+1}] + m_i}{h_i^2}.$$

- **Piecewise cubic Hermite interpolation** — Choose $m_i = f'(x_i)$ (when derivatives are known). Strictly local scheme; error $$\|f - s_3(f;\cdot)\|_\infty \le \frac{1}{384}|\Delta|^4 \|f^{(4)}\|_\infty,$$ giving $O(n^{-4})$ for equally spaced nodes ($|\Delta| = (b-a)/(n-1)$).

- **Cubic spline via second derivatives / moments ($s_3 \in \mathbb{S}_3^2(\Delta)$)** — Require continuity of second derivatives at interior nodes. Prescribe **moments** $M_1, \dots, M_n$ with $s_3''(f;x_i) = M_i$. Since $s_3''$ is linear on each piece, it interpolates the moments linearly.

- **Slope (tridiagonal) system for $\mathbb{S}_3^2$** — From $C^2$ continuity, $$h_i m_{i-1} + 2(h_{i-1}+h_i)m_i + h_{i-1}m_{i+1} = b_i,\quad i = 2,\dots,n-1,$$ where $b_i = 3\big(h_i f[x_{i-1},x_i] + h_{i-1} f[x_i, x_{i+1}]\big)$. With $m_1, m_n$ fixed it is a tridiagonal system in $m_1, \dots, m_n$.

- **Moment system** — $$h_{i-1} M_{i-1} + 2(h_{i-1}+h_i)M_i + h_i M_{i+1} = 6\big(f[x_i,x_{i+1}] - f[x_{i-1},x_i]\big),\quad i = \overline{2,n-1}.$$ Two extra conditions on $M_1, M_n$ close the system.

- **Weight function $w$** — Continuous case: $w : [a,b] \to \mathbb{R}_+$ with (i) $\int_a^b |x|^n w(x)\,dx$ finite for all $n \ge 0$, and (ii) $\int_a^b w(x)g(x)\,dx = 0$ with $g \ge 0 \Rightarrow g \equiv 0$. Discrete case: $w_i \ge 0$ with analogous conditions. Common weights: $w \equiv 1$, $\frac{1}{\sqrt{1-x^2}}$, $\sqrt{1-x^2}$ on $[-1,1]$; $e^{-x}$ on $[0,\infty)$; $e^{-x^2}$ on $\mathbb{R}$.

- **Scalar (inner) product** — $$\langle u,v\rangle = \int_a^b w(x)u(x)v(x)\,dx \quad\text{(continuous)},\qquad \langle u,v\rangle = \sum_{i=1}^N w_i u_i v_i \quad\text{(discrete)},$$ with norm $\|u\| = \langle u,u\rangle^{1/2}$. Properties: symmetry, homogeneity, additivity, positive definiteness, Cauchy–Bunyakovsky–Schwarz $|\langle u,v\rangle| \le \|u\|\,\|v\|$.

- **Orthogonal / orthonormal systems** — $u, v$ orthogonal if $\langle u,v\rangle = 0$; family $\{u_k\}$ orthogonal if $\langle u_i,u_j\rangle = 0$ for $i \neq j$; orthonormal if $\langle u_i,u_j\rangle = \delta_{ij}$.

- **Best approximation problem** — Given $f \in X$, family $\Phi = \Phi_m = \{\varphi = \sum_{j=1}^m c_j \pi_j\}$, and a norm, find $\varphi^* \in \Phi$ with $\|f - \varphi^*\| \le \|f - \varphi\|$ for all $\varphi \in \Phi$.

- **Least squares (mean square) approximation** — Take the 2-norm and minimize $$E^2(\varphi) = \|\varphi - f\|^2 = \|\varphi\|^2 - 2\langle\varphi,f\rangle + \|f\|^2.$$ Solution given by Gauss and Legendre (early 19th century).

- **Normal equations** — Setting $\partial E^2 / \partial c_i = 0$ gives $$\sum_{j=1}^m \langle\pi_i,\pi_j\rangle c_j = \langle\pi_i,f\rangle,\quad i = 1,\dots,m,$$ a linear system $Ac = b$ with $a_{ij} = \langle\pi_i,\pi_j\rangle$, $b_i = \langle\pi_i,f\rangle$.

- **Data fitting** — Discrete least squares approximation of scattered data.

- **Three-term recurrence for orthogonal polynomials** — $$\pi_{k+1}(t) = (t - \alpha_k)\pi_k(t) - \beta_k \pi_{k-1}(t),\quad \pi_{-1} \equiv 0,\ \pi_0 \equiv 1,$$ with $\alpha_k = \frac{\langle t\pi_k, \pi_k\rangle}{\|\pi_k\|^2}$, $\beta_k = \frac{\|\pi_k\|^2}{\|\pi_{k-1}\|^2}$ ($k \ge 1$), $\beta_0 = \mu_0$.

## Theorems & Results

### Boundary-condition variants for cubic splines
1. **Complete (clamped) splines:** $m_1 = f'(a)$, $m_n = f'(b)$. If $f \in C^4[a,b]$, then $\|f^{(r)} - s_3^{(r)}\|_\infty \le C_r |\Delta|^{4-r}\|f^{(4)}\|_\infty$, $r = 0,1,2,3$, with $C_0 = \frac{5}{384}$, $C_1 = \frac{1}{24}$, $C_2 = \frac{3}{8}$ ($C_3$ depends on $|\Delta|/\min_i h_i$).
2. **Endpoint second-derivative splines:** $s_3''(f;a) = f''(a)$, $s_3''(f;b) = f''(b)$, giving $2m_1 + m_2 = 3f[x_1,x_2] - \frac{1}{2}f''(a)h_1$ and $m_{n-1} + 2m_n = 3f[x_{n-1},x_n] - \frac{1}{2}f''(b)h_{n-1}$, placed first/last to keep the system tridiagonal.
3. **Natural cubic splines:** $s_3''(f;a) = s_3''(f;b) = 0$, i.e. the above with $f''(a)=f''(b)=0$: $2m_1 + m_2 = 3f[x_1,x_2]$, $m_{n-1} + 2m_n = 3f[x_{n-1},x_n]$. Needs only function values, but accuracy drops to $O(|\Delta|^2)$ near endpoints. Physical motivation: a flexible beam is straight outside $[x_1,x_n]$, so $s'' = 0$ there.
4. **Not-a-knot (deBoor) splines:** impose $p_1 \equiv p_2$ and $p_{n-2} \equiv p_{n-1}$ (interior nodes $x_2, x_{n-1}$ inactive), i.e. equality of leading coefficients $c_{1,3} = c_{2,3}$, $c_{n-2,3} = c_{n-1,3}$. The two extra equations can be combined to restore a tridiagonal system. Requires only function values.

For the moment formulation, $M_1 = M_n = 0$ gives the natural spline; the equations $2M_1 + M_2 = 6(f[x_1,x_2] - f_1')$, $M_{n-1} + 2M_n = 6(f_n' - f[x_{n-1},x_n])$ give the complete spline.

### Theorem 2.2 (minimality of the natural spline)
Let $g \in C^2[a,b]$ be any function interpolating $f$ on $\Delta$. Then $$\int_a^b |s_{nat}''(f;x)|^2\,dx \le \int_a^b |g''(x)|^2\,dx,$$ with equality iff $g = s_{nat}(f;\cdot)$. The natural cubic spline minimizes total curvature among all $C^2$ interpolants.

### Theorem 2.3 (minimality of the complete spline)
On the grid $\Delta'$ with double endpoints ($a = x_0 = x_1 < \cdots < x_n = x_{n+1} = b$, interpolating function values at interior points and both function and derivative values at endpoints), for any $g \in C^2[a,b]$ interpolating $f$ on $\Delta'$, $$\int_a^b |s_{compl}''(f;x)|^2\,dx \le \int_a^b |g''(x)|^2\,dx,$$ with equality iff $g = s_{compl}(f;\cdot)$.

### Remark 2.4 / 2.5
Taking $g = s_{compl}$ in Theorem 2.2 gives $\int_a^b |s_{nat}''|^2 \le \int_a^b |s_{compl}''|^2$, so the natural spline is the "smoothest" interpolant. The name "spline" comes from a flexible strip of wood used for drawing curves (these minimality/bending properties).

### Existence and uniqueness of the least squares solution
The normal-equations matrix $A$ ($a_{ij} = \langle\pi_i,\pi_j\rangle$) is symmetric and positive definite: for $x \neq 0$, $x^T A x = \big\|\sum_i x_i \pi_i\big\|^2 > 0$. Hence $A$ is nonsingular, the system $Ac = b$ has a unique solution $c_j^*$, and the least squares approximation $\varphi^*(t) = \sum_j c_j^* \pi_j(t)$ exists uniquely.

### Orthogonal basis diagonalizes the normal equations
If $\{\pi_j\}$ is orthogonal then $a_{ij} = 0$ for $i \neq j$, so $Ac = b$ is diagonal with $$c_j^* = \frac{\langle\pi_j, f\rangle}{\langle\pi_j,\pi_j\rangle},\qquad \varphi^*(t) = \sum_{j=1}^m \frac{\langle\pi_j,f\rangle}{\langle\pi_j,\pi_j\rangle}\pi_j(t).$$
**Why this matters.** With the canonical basis $\pi_j = t^{j-1}$ on $[0,1]$, $A = H_m$ is the **Hilbert matrix** ($a_{ij} = \frac{1}{i+j-1}$), severely ill-conditioned ($m \ge 4$ gives unusable solutions). Orthogonal bases avoid this and give **permanence of coefficients**: $c_j^*$ are independent of $m$, so increasing $m$ reuses prior computations. Gram–Schmidt on $1, t, t^2, \dots$ with weight $w$ produces the classical families.

### Classical orthogonal polynomials (Table 2)
- **Legendre** $l_m$: weight $1$ on $[-1,1]$, $\alpha_k = 0$, $\beta_0 = 2$, $\beta_k = (4 - k^{-2})^{-1}$.
- **Chebyshev 1st kind** $T_m = \cos(m\arccos x)$: weight $(1-x^2)^{-1/2}$ on $[-1,1]$, $\alpha_k=0$, $\beta_0=\pi$, $\beta_1=\frac12$, $\beta_k=\frac14$ ($k\ge2$).
- **Chebyshev 2nd kind** $Q_m$: weight $(1-x^2)^{1/2}$ on $[-1,1]$, $\beta_0 = \frac{\pi}{2}$, $\beta_k = \frac14$.
- **Laguerre** $L_m^a$: weight $x^a e^{-x}$ on $[0,\infty)$, $\alpha_k = 2k + a + 1$, $\beta_0 = \Gamma(1+a)$, $\beta_k = k(k+a)$.
- **Hermite** $H_m$: weight $e^{-x^2}$ on $\mathbb{R}$, $\alpha_k = 0$, $\beta_0 = \sqrt\pi$, $\beta_k = \frac{k}{2}$.

## Example Problems

### Example: Natural cubic spline through $(1,3),(2,5),(4,9),(5,10)$ (Example 2.1)
$n = 4$, $h_1 = 1, h_2 = 2, h_3 = 1$. Solving the slope system (with natural end conditions) gives $m_1 = \frac{87}{46}, m_2 = \frac{51}{23}, m_3 = \frac{21}{23}, m_4 = \frac{24}{23}$. The moment system with $M_1 = M_4 = 0$ gives $M_1 = 0, M_2 = \frac{3}{8}, M_3 = -\frac{9}{8}, M_4 = 0$. Both routes yield $$s_3(x) = \begin{cases} \frac{x^3}{16} - \frac{3x^2}{16} + \frac{17x}{8} + 1, & x \in [1,2] \\[1mm] -\frac{x^3}{8} + \frac{15x^2}{16} - \frac{x}{8} + \frac{5}{2}, & x \in [2,4] \\[1mm] \frac{3x^3}{16} - \frac{45x^2}{16} + \frac{119x}{8} - \frac{35}{2}, & x \in [4,5] \end{cases}$$ This uses the **slope system**, the **moment system**, and the **natural boundary conditions** above.

### Example: Comparing spline types on $f(x) = \arctan x$ (Example 2.6)
On $[-2,2]$ with nodes $\{-2,-1,0,1,2\}$, the complete, natural, deBoor, and piecewise-Hermite cubic splines all track $f$ closely. The error plot shows the deBoor spline with the largest error near endpoints and the complete/natural splines smallest in the interior — illustrating the **boundary-condition variants** and their accuracy trade-offs (Theorem 2.2/2.3, the $O(|\Delta|^2)$ endpoint penalty).

### Example: Continuous linear least squares of $f = \cos\pi t$ on $[0,1]$ (Example 3.4)
Basis $\pi_1 = 1, \pi_2 = t$, seek $\varphi = a + bt$. Compute $\langle\pi_1,\pi_1\rangle = 1$, $\langle\pi_1,\pi_2\rangle = \frac12$, $\langle\pi_2,\pi_2\rangle = \frac13$, $\langle\pi_1,f\rangle = 0$, $\langle\pi_2,f\rangle = -\frac{2}{\pi^2}$ (last via integration by parts). The **normal equations** $a + \frac12 b = 0$, $\frac12 a + \frac13 b = -\frac{2}{\pi^2}$ give $a = \frac{12}{\pi^2}, b = -\frac{24}{\pi^2}$, so $$\varphi^*(t) = \frac{12}{\pi^2}(1 - 2t).$$ This directly applies the **normal equations** with a continuous scalar product.

### Example: Quadratic data fitting (Example 3.5)
Fit $\varphi(x) = a + bx + cx^2$ to the six points $x = (-5,-3,-1,1,3,5)$, $y = (4.8,3.0,2.0,2.8,5.2,10)$. The **discrete normal equations** with $\langle\pi_i,\pi_j\rangle = \sum_k x_k^{i+j-2}$ reduce (using the summed table) to
$$\begin{cases} 6a + 70c = 27.8 \\ 70b = 33.4 \\ 70a + 1414c = 448.6 \end{cases}$$
with solution $a = 2.206, b = 0.477, c = 0.208$, so $\varphi^*(x) = 0.208x^2 + 0.477x + 2.206$. This is **data fitting** via the normal equations.

### Example: Least squares of $f = \arccos t$ with Chebyshev basis (Example 3.6)
On $[-1,1]$ with weight $w = \frac{1}{\sqrt{1-t^2}}$, use $\pi_j = T_j$ (orthogonal). Then $\langle\pi_0,\pi_0\rangle = \pi$, $\langle\pi_j,\pi_j\rangle = \frac{\pi}{2}$ ($j \neq 0$). The numerators $\langle\pi_j,f\rangle = \int_0^\pi x\cos(jx)\,dx$ give $\frac{\pi^2}{2}$ for $j=0$ and $\frac{1}{j^2}((-1)^j - 1)$ otherwise ($0$ for even $j$, $-\frac{2}{j^2}$ for odd). Hence $c_0^* = \frac{\pi}{2}$, $c_j^* = -\frac{4}{j^2\pi}$ for odd $j$ (else $0$), giving $$\varphi^*_{2n+1}(t) = \frac{\pi}{2} - \sum_{i=0}^n \frac{4}{(2i+1)^2\pi}\,T_{2i+1}(t).$$ This uses the **orthogonal-basis formula** $c_j^* = \langle\pi_j,f\rangle/\langle\pi_j,\pi_j\rangle$ and the **Chebyshev** family from Table 2.

## Cheat-sheet
- Cubic piece (Taylor form): coefficients $c_{i,0}=f_i$, $c_{i,1}=m_i$, $c_{i,2}=\frac{3f[x_i,x_{i+1}]-2m_i-m_{i+1}}{h_i}$, $c_{i,3}=\frac{m_{i+1}-2f[x_i,x_{i+1}]+m_i}{h_i^2}$.
- Piecewise Hermite ($m_i=f'(x_i)$) error: $\frac{1}{384}|\Delta|^4\|f^{(4)}\|_\infty = O(n^{-4})$.
- Slope system ($\mathbb{S}_3^2$): $h_i m_{i-1} + 2(h_{i-1}+h_i)m_i + h_{i-1}m_{i+1} = 3(h_i f[x_{i-1},x_i] + h_{i-1}f[x_i,x_{i+1}])$.
- Moment system: $h_{i-1}M_{i-1} + 2(h_{i-1}+h_i)M_i + h_i M_{i+1} = 6(f[x_i,x_{i+1}] - f[x_{i-1},x_i])$.
- Boundary types: complete ($m_1=f'(a), m_n=f'(b)$), natural ($s''=0$ at ends), endpoint-$f''$, not-a-knot/deBoor ($p_1\equiv p_2$, $p_{n-2}\equiv p_{n-1}$).
- Complete-spline error constants: $C_0=\frac{5}{384}, C_1=\frac{1}{24}, C_2=\frac{3}{8}$.
- Natural spline minimizes $\int_a^b |s''|^2$ (Theorem 2.2); $\int |s_{nat}''|^2 \le \int |s_{compl}''|^2$.
- Inner product: $\langle u,v\rangle = \int_a^b w u v\,dx$ or $\sum_i w_i u_i v_i$; $\|u\| = \langle u,u\rangle^{1/2}$; CBS: $|\langle u,v\rangle| \le \|u\|\|v\|$.
- Least squares error: $E^2(\varphi) = \|\varphi\|^2 - 2\langle\varphi,f\rangle + \|f\|^2$.
- Normal equations: $Ac=b$, $a_{ij}=\langle\pi_i,\pi_j\rangle$, $b_i=\langle\pi_i,f\rangle$; $A$ symmetric positive definite.
- Canonical basis $t^{j-1}$ on $[0,1]$ $\Rightarrow$ Hilbert matrix $a_{ij}=\frac{1}{i+j-1}$ (ill-conditioned).
- Orthogonal basis $\Rightarrow$ $c_j^* = \frac{\langle\pi_j,f\rangle}{\langle\pi_j,\pi_j\rangle}$, permanence of coefficients.
- Recurrence: $\pi_{k+1} = (t-\alpha_k)\pi_k - \beta_k\pi_{k-1}$, $\alpha_k = \frac{\langle t\pi_k,\pi_k\rangle}{\|\pi_k\|^2}$, $\beta_k = \frac{\|\pi_k\|^2}{\|\pi_{k-1}\|^2}$.
- Classical families: Legendre, Chebyshev 1st/2nd, Laguerre, Hermite (see weights/intervals in Table 2).
