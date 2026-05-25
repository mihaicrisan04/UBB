# Lecture 5 — Newton-type and Aitken-type Interpolation Methods

## Overview
This lecture develops alternative, computationally cheaper ways to build the (unique) polynomial interpolant introduced earlier with Lagrange's formula. It covers Newton's divided-difference form (variable step), Newton's forward/backward finite-difference forms (constant step, equidistant nodes), and the iterative Aitken/Neville schemes that build a degree-$n$ interpolant recursively from degree-$(n-1)$ pieces. The key practical payoff is being able to add a node incrementally and to interpolate efficiently in tabulated data.

## Key Notions & Definitions

- **Monic node polynomial $\psi_n$** — the monic polynomial of the nodes, emphasizing the node count:
$$\psi_n(x) = (x-x_0)\dots(x-x_{n-1})(x-x_n), \qquad \psi_{n-1}(x) = (x-x_0)\dots(x-x_{n-1}).$$
It satisfies the recurrence $\psi_n(x) = (x-x_n)\psi_{n-1}(x)$ and $\psi_n'(x) = \psi_{n-1}(x) + (x-x_n)\psi_{n-1}'(x)$, hence $\psi_n'(x_i) = (x_i-x_n)\psi_{n-1}'(x_i)$ for $i=0,\dots,n-1$ and $\psi_n'(x_n) = \psi_{n-1}(x_n)$.

- **Divided difference (sum form)** — recalled from Lecture 2:
$$f[x_0,\dots,x_n] = \sum_{i=0}^{n} \frac{1}{\psi_n'(x_i)} f_i.$$

- **Newton's divided difference form** $N_n f$ — an alternative writing of the interpolation polynomial. By uniqueness $L_n f(x) = N_n f(x)$ (same polynomial, different form):
$$N_n f(x) = f(x_0) + f[x_0,x_1](x-x_0) + \dots + f[x_0,\dots,x_n](x-x_0)\dots(x-x_{n-1}).$$

- **Nested (Horner) form** — with $D_i = f[x_0,\dots,x_i]$:
$$N_n f(x) = D_0 + (x-x_0)\Big[D_1 + (x-x_1)\big[D_2 + \dots + (x-x_{n-2})[D_{n-1}+(x-x_{n-1})D_n]\dots\big]\Big].$$
Evaluation needs only $n$ multiplications and $n$ additions once the divided differences are known.

- **Forward difference $\Delta$** (equidistant nodes $x_i = x_0 + ih$):
$$\Delta^1 f(x_i) = f_{i+1}-f_i, \qquad \Delta^k f(x_i) = \Delta^{k-1} f_{i+1} - \Delta^{k-1} f_i.$$

- **Backward difference $\nabla$**:
$$\nabla^0 f_i = f_i, \quad \nabla^1 f_i = f_i - f_{i-1}, \quad \nabla^k f_i = \nabla^{k-1} f_i - \nabla^{k-1} f_{i-1}.$$

- **Link between divided and finite differences** (Proposition 5.6, Lecture 2):
$$f[x_0, x_0+h, \dots, x_0+ih] = \frac{1}{i!\,h^i}\Delta^i f_0.$$

- **Generalized binomial coefficient** — for $s\in\mathbb{R}$, $k\in\mathbb{N}$:
$$\binom{s}{k} = \frac{s(s-1)\cdots(s-k+1)}{k!}.$$

- **Aitken/Neville interpolants $\overline{P}$** — $\overline{P}_{0\dots k}$ is the polynomial of degree $k$ interpolating $f$ at $x_0,\dots,x_k$; built from two degree-$(k-1)$ interpolants on overlapping node subsets.

## Theorems & Results

### Recurrence for the Newton form (adding a node)
Let $Q(x) = L_n f(x) - L_{n-1}f(x)$. Since $Q(x_i)=0$ for $i=0,\dots,n-1$, $Q$ has those $n$ roots, so $Q(x) = a_n \psi_{n-1}(x)$. Imposing $L_n f(x_n)=f_n$ gives $Q(x_n)=f_n - L_{n-1}f(x_n)$, and using the divided-difference identities one obtains $a_n = f[x_0,\dots,x_n]$. Hence:
$$L_n f(x) = L_{n-1}f(x) + f[x_0,\dots,x_n]\,\psi_{n-1}(x), \quad n\ge 1.$$
Iterating from $L_0 f = f(x_0)$ reproduces the Newton divided-difference form $N_n f$.

### Newton form of the remainder
For fixed $x$ in the smallest interval $[a,b]$ containing the distinct nodes, recursively expanding the divided differences $f[x,x_0,\dots]$ gives
$$R_n f(x) = f[x,x_0,\dots,x_n](x-x_0)\dots(x-x_n).$$
By the mean-value formula for divided differences (Theorem 5.1e, Lecture 2), this equals the classical remainder
$$R_n f(x) = \frac{(x-x_0)\dots(x-x_n)}{(n+1)!}f^{(n+1)}(\xi), \qquad \xi\in(a,b).$$

### Newton's forward difference formula
With $s = (x-x_0)/h$:
$$L_n f(x) = \sum_{i=0}^{n}\binom{s}{i}\Delta^i f_0 = f_0 + \binom{s}{1}\Delta f_0 + \binom{s}{2}\Delta^2 f_0 + \dots + \binom{s}{n}\Delta^n f_0,$$
with error (for $x = x_0 + sh$)
$$f(x) - L_n f(x) = h^{n+1}\binom{s}{n+1}f^{(n+1)}(\xi_x),$$
$\xi_x$ in the smallest interval containing $x_0,\dots,x_n$ and $x$.

### Newton's backward difference formula
With $t = (x-x_n)/h$:
$$L_n f(x) = f_n + \binom{t}{1}\nabla f_n + \binom{t+1}{2}\nabla^2 f_n + \dots + \binom{t+n-1}{n}\nabla^n f_n,$$
with error
$$f(x) - L_n f(x) = h^{n+1}\binom{t+n}{n+1}f^{(n+1)}(\eta_x).$$

**Remark 1.4 (preferential precision zones).** Variable-step methods (Lagrange, barycentric, Newton divided differences) have the same precision anywhere in the interval — no preferential zone. Constant-step methods do: the forward formula is more precise near the first node $(x_0,f_0)$; the backward formula is preferred near the last node $(x_n,f_n)$.

### Proposition 1.5 (Neville recurrence)
Let $x_0,\dots,x_k$ be distinct nodes with values $f_i$. The Lagrange interpolant is
$$\overline{P}_{0\dots k}(x) = \frac{(x-x_0)\overline{P}_{12\dots k}(x) - (x-x_k)\overline{P}_{01\dots k-1}(x)}{x_k - x_0}.$$
*Proof sketch:* the right side has degree $k$ by construction; checking its values at $x_0$, at interior $x_j$, and at $x_k$ shows it equals $f_0, f_j, f_k$ respectively, so by uniqueness it is $L_k f$.

### Neville's method (in $P_{i,j}$ notation)
Define $P_{i,j} = \overline{P}_{i-j,\dots,i}$ (degree-$j$ interpolant on nodes $x_{i-j},\dots,x_i$):
$$P_{i,0} = f(x_i), \qquad P_{i,j} = \frac{(x-x_{i-j})P_{i,j-1} - (x-x_i)P_{i-1,j-1}}{x_i - x_{i-j}}, \quad i\ge j>0.$$
The interpolant is the diagonal value $L_n f = P_{nn}$. Stopping criterion if convergent: $|P_{ii} - P_{i-1,i-1}| < \varepsilon$.

### Aitken's method
Similar table $Q_{i,j}$:
$$Q_{i,0} = f(x_i), \qquad Q_{i,j+1} = \frac{(x-x_j)Q_{i,j} - (x-x_i)Q_{j,j}}{x_i - x_j}, \quad i>j\ge 0,$$
again with $L_n f = Q_{nn}$. Aitken and Neville give the same main-diagonal values.

## Example Problems

### Example: Linear and quadratic Newton interpolation (Example 1.1)
**Problem.** Data $(0.1,0.2),(0.2,0.24),(0.3,0.3)$. Find $N_1(0.15)$, $N_2(0.15)$ and their remainders.
**Solution.** Divided differences: $f[x_0]=0.2$, $f[x_0,x_1]=\frac{0.24-0.2}{0.1}=0.4$, $f[x_1,x_2]=\frac{0.3-0.24}{0.1}=0.6$, $f[x_0,x_1,x_2]=\frac{0.6-0.4}{0.2}=1$.
Linear: $N_1 f(x)=0.2+0.4(x-0.1)=0.4x+0.16$, so $f(0.15)\approx 0.22$, with $R_1 f(0.15)=\frac{(0.15-0.1)(0.15-0.2)}{2!}f''(\xi)=-1.25\cdot10^{-3}f''(\xi)$.
Quadratic: $N_2 f(x)=0.2+0.4(x-0.1)+1\cdot(x-0.1)(x-0.2)=x^2+0.1x+0.18$, so $f(0.15)\approx 0.2175$, with $R_2 f(0.15)=\frac{(0.15-0.1)(0.15-0.2)(0.15-0.3)}{3!}f'''(\eta)=6.25\cdot10^{-5}f'''(\eta)$.
*This uses the Newton divided-difference form and the Newton remainder formula above.*

### Example: Minimum-degree interpolant of $f(-1),f(0),f(2)$ (Example 1.2)
**Problem.** For $f\in C^3[-1,2]$, find the minimum-degree polynomial through $f(-1),f(0),f(2)$ and discuss the remainder.
**Solution.** Divided differences give $f[-1,0]=f(0)-f(-1)$, $f[0,2]=\tfrac12(f(2)-f(0))$, $f[-1,0,2]=\tfrac16(2f(-1)-3f(0)+f(2))$. Then
$$L_2 f(x) = f(-1) + (f(0)-f(-1))(x+1) + \tfrac16(2f(-1)-3f(0)+f(2))\,x(x+1).$$
Rewriting as a linear combination of values exposes the Lagrange basis: $L_2 f(x)=\tfrac13 x(x-2)f(-1)-\tfrac12(x+1)(x-2)f(0)+\tfrac16 x(x+1)f(2)$. Remainder $R_2 f(x)=\frac{x(x+1)(x-2)}{3!}f'''(\xi)$. With $u(x)=x(x+1)(x-2)=x^3-x^2-2x$, the critical points are $\frac{1\pm\sqrt7}{3}$, and $\max_{[0,2]}|u| = |u(\tfrac{1+\sqrt7}{3})| = \tfrac{2}{27}(10+7\sqrt7)$, giving the bound $|R_2 f(x)|\le \frac{2}{27\cdot 3!}(10+7\sqrt7)|f'''(\xi)|\approx 0.3521\,|f'''(\xi)|$.
*This illustrates the Newton form plus bounding the node-polynomial in the remainder.*

### Example: Same data via finite differences (Example 1.3)
**Problem.** Same data as Example 1.1; compute $L_2 f(0.15)$ using forward and backward differences ($h=0.1$).
**Solution.** Forward differences: $\Delta f_0=0.04$, $\Delta f_1=0.06$, $\Delta^2 f_0=0.02$. With $s=(x-x_0)/h$,
$$L_2 f(x)=0.2+\tfrac{x-0.1}{0.1}(0.04)+\tfrac{(x-0.1)(x-0.2)}{0.02}(0.02)=x^2+0.1x+0.18,$$
so $L_2 f(0.15)=0.2175$. Backward differences: $\nabla f_1=0.04$, $\nabla f_2=0.06$, $\nabla^2 f_2=0.02$; with $t=(x-x_2)/h$ the backward formula gives the same polynomial and value $0.2175$.
*This applies Newton's forward and backward difference formulas; the result matches the divided-difference form, confirming uniqueness.*

### Example: Approximating $\sqrt2$ with Neville and Aitken (Example 1.6)
**Problem.** Approximate $\sqrt2$ by interpolating $f(x)=2^x$ at nodes $-1,0,1$ then $-1,0,1,2$, evaluated at $x=1/2$.
**Solution (Neville).** Base values $P_{00}=1/2$, $P_{10}=1$, $P_{20}=2$. The recurrence gives $P_{11}=5/4$, $P_{21}=3/2$, $P_{22}=23/16=1.4375$, so $\sqrt2\approx 1.4375$ with diagonal check $|P_{22}-P_{11}|=3/16=0.1875$. Adding $x_3=2$ ($P_{30}=4$) yields $P_{31}=1$, $P_{32}=11/8$, $P_{33}=45/32=1.4063$, with $|P_{33}-P_{22}|=1/32=0.0313$. True $\sqrt2\approx1.4142$, so actual errors $0.0233$ and $0.0079$.
**Aitken** produces the same diagonal: $Q_{22}=23/16$, $Q_{33}=45/32$ (intermediate off-diagonal entries differ, e.g. $Q_{21}=13/8$, $Q_{31}=9/4$).
*This exercises Proposition 1.5 and both iterative schemes; both give $L_n f$ on the diagonal.*

## Cheat-sheet
- Node-polynomial recurrence: $\psi_n=(x-x_n)\psi_{n-1}$; $\psi_n'(x_i)=(x_i-x_n)\psi_{n-1}'(x_i)$, $\psi_n'(x_n)=\psi_{n-1}(x_n)$.
- Newton update: $L_n f = L_{n-1}f + f[x_0,\dots,x_n]\psi_{n-1}(x)$.
- Newton form: $N_n f(x)=\sum_{i=0}^n f[x_0,\dots,x_i](x-x_0)\dots(x-x_{i-1})$; evaluate with the nested (Horner) form ($n$ mults, $n$ adds).
- Remainder: $R_n f(x)=f[x,x_0,\dots,x_n]\prod_i(x-x_i)=\frac{\prod_i(x-x_i)}{(n+1)!}f^{(n+1)}(\xi)$.
- Equidistant link: $f[x_0,\dots,x_0+ih]=\frac{1}{i!h^i}\Delta^i f_0$.
- Forward: $L_n f=\sum_{i=0}^n\binom{s}{i}\Delta^i f_0$, $s=(x-x_0)/h$; error $h^{n+1}\binom{s}{n+1}f^{(n+1)}(\xi_x)$. Best near $x_0$.
- Backward: $L_n f=f_n+\sum_{i=1}^n\binom{t+i-1}{i}\nabla^i f_n$, $t=(x-x_n)/h$; error $h^{n+1}\binom{t+n}{n+1}f^{(n+1)}(\eta_x)$. Best near $x_n$.
- Neville: $P_{i,j}=\frac{(x-x_{i-j})P_{i,j-1}-(x-x_i)P_{i-1,j-1}}{x_i-x_{i-j}}$, $L_nf=P_{nn}$, stop on $|P_{ii}-P_{i-1,i-1}|<\varepsilon$.
- Aitken: $Q_{i,j+1}=\frac{(x-x_j)Q_{i,j}-(x-x_i)Q_{j,j}}{x_i-x_j}$, $L_nf=Q_{nn}$.
- Generalized binomial: $\binom{s}{k}=\frac{s(s-1)\cdots(s-k+1)}{k!}$.
