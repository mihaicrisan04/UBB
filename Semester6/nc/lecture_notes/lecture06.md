# Lecture 6 — Hermite Interpolation

## Overview
This lecture extends polynomial interpolation to the case where not only function values but also derivative values are prescribed at the nodes (e.g. knowing position and velocity of a moving object). It builds the Hermite interpolation polynomial via fundamental polynomials, via Newton's divided differences with repeated nodes, and in the general case of arbitrary node multiplicities. The error formula generalizes the Lagrange remainder, and special cases recover the Lagrange and Taylor polynomials.

## Key Notions & Definitions

- **Hermite interpolation problem with double nodes** — given $m+1$ distinct nodes $x_i$ and the values $f(x_i), f'(x_i)$, find the minimum-degree polynomial $P$ with
$$P(x_i) = f(x_i), \qquad P'(x_i) = f'(x_i), \quad i=\overline{0,m}.$$
There are $2m+2$ conditions, so $\deg P \le n = 2m+1$. The nodes are called *double* nodes.

- **Lagrange fundamental polynomials** (recalled): $l_i(x) = \frac{\psi_m(x)}{(x-x_i)\psi_m'(x_i)}$, with $l_i(x_j)=\delta_{ij}$, and $L_m f(x)=\sum_{i=0}^m l_i(x)f(x_i)$.

- **Hermite fundamental (basis) polynomials** (double nodes):
$$h_{i0}(x) = \big[1 - 2l_i'(x_i)(x-x_i)\big]\,[l_i(x)]^2, \qquad h_{i1}(x) = (x-x_i)\,[l_i(x)]^2.$$
They satisfy $h_{i0}(x_j)=\delta_{ij}$, $h_{i0}'(x_j)=0$, $h_{i1}(x_j)=0$, $h_{i1}'(x_j)=\delta_{ij}$.

- **General Hermite problem** — $m+1$ distinct nodes $x_i$ with multiplicity $r_i+1$, given the derivatives $f^{(j)}(x_i)$, $j=\overline{0,r_i}$. Find minimum-degree $P$ with
$$P^{(j)}(x_i) = f^{(j)}(x_i), \quad i=\overline{0,m},\ j=\overline{0,r_i}.$$
Total conditions $n+1 = \sum_{i=0}^m (r_i+1)$, so $\deg P \le n$.

- **General Hermite fundamental polynomials** $h_{ij}$ — satisfy $h_{ij}^{(k)}(x_l)=0$ for $l\ne i$, and $h_{ij}^{(k)}(x_i)=\delta_{jk}$. With $u(x)=\prod_{i}(x-x_i)^{r_i+1}$ and $u_i(x)=u(x)/(x-x_i)^{r_i+1}$:
$$h_{ij}(x) = \frac{(x-x_i)^j}{j!}\left[\sum_{k=0}^{r_i-j}\frac{(x-x_i)^k}{k!}\Big[\frac{1}{u_i(x)}\Big]^{(k)}_{x=x_i}\right]u_i(x).$$

- **Generalized Vandermonde matrix** — for the direct (linear-system) construction, e.g. with a double node $x_1$ and simple $x_0$ the matrix of coefficients of $ax^2+bx+c$ is $V=\begin{bmatrix} x_0^2 & x_0 & 1\\ x_1^2 & x_1 & 1\\ 2x_1 & 1 & 0\end{bmatrix}$; it is invertible and its inverse gives the fundamental-polynomial coefficients.

## Theorems & Results

### Theorem 1.1 (existence and uniqueness, double nodes)
There is a unique polynomial $H_n f$ of degree at most $n=2m+1$ satisfying the double-node conditions, given by
$$H_n f(x) = \sum_{i=0}^m \big[h_{i0}(x)f(x_i) + h_{i1}(x)f'(x_i)\big].$$
*Proof sketch.* **Existence:** $\deg h_{i0}=\deg h_{i1}=2m+1$. Using $l_i(x_j)=\delta_{ij}$ one verifies the interpolation values and the derivative values of $h_{i0},h_{i1}$ at the nodes, so $H_n f$ matches $f(x_k)$ and $f'(x_k)$. **Uniqueness:** if $G_n$ also satisfies the conditions, $Q_n=H_n-G_n$ has degree $\le 2m+1$ and vanishes with its derivative at all $m+1$ nodes — i.e. $m+1$ *double* roots, $2m+2$ roots total. By the Fundamental Theorem of Algebra $Q_n\equiv 0$.

### Proposition 1.3 (Newton form and error, double nodes)
Take $2m+2$ nodes $z_0,\dots,z_{2m+1}$ and the Newton interpolant; let pairs collapse $z_{2i},z_{2i+1}\to x_i$. With $n=2m+1$,
$$N_n f(x) = f(x_0)+f[x_0,x_0](x-x_0)+f[x_0,x_0,x_1](x-x_0)^2+\dots+f[x_0,x_0,\dots,x_m,x_m](x-x_0)^2\dots(x-x_m),$$
where repeated-argument divided differences use $f[x_i,x_i]=f'(x_i)$. If $f\in C^{2m+2}[a,b]$, then $H_n f = N_n f$ and the error is
$$R_n(x) = f(x) - H_n f(x) = [\psi_m(x)]^2\,\frac{f^{(n+1)}(\xi_x)}{(n+1)!}, \qquad \xi_x\in(a,b).$$
*Proof sketch.* $N_n$ has degree $\le n$; from $R_{2m+1}(x)=f[x,z_0,\dots,z_{2m+1}]\prod(x-z_i)$ taking limits shows $f(x_i)-N_n(x_i)=0$ and $f'(x_i)-N_n'(x_i)=0$, so by uniqueness $H_n f=N_n f$; the error follows from the mean-value formula for divided differences.

### Theorem 1.6 (existence and uniqueness, general case)
There is a unique polynomial $H_n f$ of degree at most $n$ (with $n+1=\sum_i(r_i+1)$) satisfying the general interpolation conditions, written as
$$H_n f(x) = \sum_{i=0}^m \sum_{j=0}^{r_i} h_{ij}(x)\,f^{(j)}(x_i).$$
The Newton form (re-indexing nodes $z_0,\dots,z_n$ by multiplicity) is
$$N_n f(x) = f(z_0)+f[z_0,z_1](x-z_0)+\dots+f[z_0,\dots,z_n](x-z_0)\dots(x-z_{n-1}),$$
with error
$$R_n(x) = \frac{u(x)}{(n+1)!}f^{(n+1)}(\xi_x), \qquad \xi_x\in(a,b),\quad u(x)=\prod_i(x-x_i)^{r_i+1}.$$

### Special cases
1. All $r_i=0$ (simple nodes) $\Rightarrow$ the Lagrange interpolation formula.
2. One node $x_0$ of multiplicity $n+1$ $\Rightarrow$ the **Taylor polynomial**:
$$H_n f(x) = T_n f(x) = \sum_{k=0}^n \frac{(x-x_0)^k}{k!}f^{(k)}(x_0), \qquad R_n(x) = \frac{(x-x_0)^{n+1}}{(n+1)!}f^{(n+1)}(\xi).$$
3. Two nodes $x_0=a$ (mult. $m+1$) and $x_1=b$ (mult. $n+1$): degree $m+n+1$; Newton form is a "two-point Taylor" expansion with remainder $R_{m+n+1}=\frac{f^{(m+n+2)}(\xi_x)}{(m+n+2)!}(x-a)^{m+1}(x-b)^{n+1}$.

## Example Problems

### Example: Cubic Hermite polynomial, two double nodes (Examples 1.2 & 1.4)
**Problem.** Two double nodes $a<b$: $P(a)=f(a),P(b)=f(b),P'(a)=f'(a),P'(b)=f'(b)$. Degree $n=2\cdot2-1=3$.
**Solution (fundamental form).** With $x_0=a,x_1=b$, $l_0=\frac{x-b}{a-b}$, $l_1=\frac{x-a}{b-a}$, the Hermite basis is
$$h_{00}=\Big[1+2\tfrac{x-a}{b-a}\Big]\Big[\tfrac{b-x}{b-a}\Big]^2,\quad h_{10}=\Big[1+2\tfrac{b-x}{b-a}\Big]\Big[\tfrac{x-a}{b-a}\Big]^2,$$
$$h_{01}=\tfrac{(x-a)(b-x)^2}{(b-a)^2},\quad h_{11}=-\tfrac{(x-a)^2(b-x)}{(b-a)^2},$$
giving $H_3 f(x)=h_{00}f(a)+h_{10}f(b)+h_{01}f'(a)+h_{11}f'(b)$.
**Newton form (Example 1.4).** With nodes $a,a,b,b$ the divided-difference table uses $f[a,a]=f'(a)$, $f[b,b]=f'(b)$, $f[a,b]=\frac{f(b)-f(a)}{b-a}$, $f[a,a,b]=\frac{f[a,b]-f'(a)}{b-a}$, $f[a,b,b]=\frac{f'(b)-f[a,b]}{b-a}$, $f[a,a,b,b]=\frac{f'(b)-2f[a,b]+f'(a)}{(b-a)^2}$, so
$$H_3 f(x)=f(a)+f'(a)(x-a)+f[a,a,b](x-a)^2+f[a,a,b,b](x-a)^2(x-b).$$
Error $R_3 f(x)=\frac{(x-a)^2(x-b)^2}{24}f^{(4)}(\xi_x)$, with bound $\max_{[a,b]}|f-H_3 f|\le \frac{(b-a)^4}{384}\max_{[a,b]}|f^{(4)}|$ (using $\max|(x-a)(x-b)|=\frac{(b-a)^2}{4}$ at the midpoint).
*This is Theorem 1.1 / Proposition 1.3 applied to two double nodes; the squared node-polynomial $[\psi_m]^2$ appears in the error.*

### Example: Lagrange vs Hermite for $\sqrt x$ (Example 1.5)
**Problem.** $f(x)=\sqrt x$, nodes $a=1,b=4$. Compare degree-1 Lagrange and cubic Hermite.
**Solution.** Lagrange: $L_1 f(x)=\tfrac13 x+\tfrac23$. Hermite uses also $f'(1)=\tfrac{1}{2\sqrt1}=\tfrac12$, $f'(4)=\tfrac{1}{2\sqrt4}=\tfrac14$. The divided-difference table gives $f[1,1]=1/2$, $f[1,4]=1/3$, $f[4,4]=1/4$, $f[1,1,4]=-1/18$, $f[1,4,4]=-1/36$, $f[1,1,4,4]=1/108$, so
$$H_3 f(x)=1+\tfrac12(x-1)-\tfrac{1}{18}(x-1)^2+\tfrac{1}{108}(x-1)^2(x-4).$$
One checks $H_3 f(1)=1,H_3 f(4)=2,(H_3 f)'(1)=\tfrac12,(H_3 f)'(4)=\tfrac14$, matching $f$ and $f'$. The plotted error of Hermite is far smaller than Lagrange across $[0.5,5]$.
*This contrasts the simple-node (Lagrange) and double-node (Hermite) interpolants, illustrating that prescribing derivatives improves accuracy.*

### Example: Simple node + double node (Example 1.8)
**Problem.** Simple node $x_0$ ($r_0+1=1$) and double node $x_1$ ($r_1+1=2$); degree $n=2$.
**Solution.** Divided-difference table with $f[x_1,x_1]=f'(x_1)$ gives top differences $f[x_0,x_1]$ and $\frac{f'(x_1)-f[x_0,x_1]}{x_1-x_0}$, so
$$H_2 f(x)=f(x_0)+f[x_0,x_1](x-x_0)+\frac{f'(x_1)-f[x_0,x_1]}{x_1-x_0}(x-x_0)(x-x_1),$$
with remainder $R_2 f(x)=\frac{(x-x_0)(x-x_1)^2}{3!}f'''(\xi)$. (If instead $x_0$ is double and $x_1$ simple: $H_2 f(x)=f(x_0)+f'(x_0)(x-x_0)+\frac{f[x_0,x_1]-f'(x_0)}{x_1-x_0}(x-x_0)^2$, $R_2 f(x)=\frac{(x-x_0)^2(x-x_1)}{3!}f'''(\xi)$.) Since $H_2 f$ has small degree it can also be found directly as $ax^2+bx+c$ via the generalized Vandermonde system.
*This is the general case (Theorem 1.6) with mixed multiplicities; note $u(x)=(x-x_0)(x-x_1)^2$ in the remainder.*

### Example: Simple node + triple node (Example 1.9)
**Problem.** Interpolate $f(0),f(1),f'(1),f''(1)$ (simple $x_0=0$, triple $x_1=1$); degree $n=1+3-1=3$.
**Solution.** Divided differences (with $f[1,1]=f'(1)$, $f[1,1,1]=\tfrac{f''(1)}{2}$) give
$$H_3 f(x)=f(0)+(f(1)-f(0))x+(f'(1)-f(1)+f(0))x(x-1)+\Big(\tfrac{f''(1)}{2}-f'(1)+f(1)-f(0)\Big)x(x-1)^2,$$
which simplifies to $H_3 f(x)=-(x-1)^3 f(0)+x(x^2-3x+3)f(1)-x(x-1)(x-2)f'(1)+\tfrac12 x(x-1)^2 f''(1)$. The fundamental polynomials are $h_{00}=-(x-1)^3$, $h_{10}=x(x^2-3x+3)$, $h_{11}=-x(x-1)(x-2)$, $h_{12}=\tfrac12 x(x-1)^2$, and one verifies the relations $h_{ij}^{(k)}(x_i)=\delta_{jk}$. Remainder $R_3 f(x)=\frac{x(x-1)^3}{4!}f^{(4)}(\xi)$; with $u(x)=x(x-1)^3$, $\max_{[0,1]}|u|=|u(1/4)|=\frac{27}{256}$, giving $|R_3 f(x)|\le\frac{27}{256\cdot 4!}\max|f^{(4)}|\approx 0.0044\,\|f^{(4)}\|$.
*This is the general-case construction with a triple node; the multiplicity-3 node contributes $(x-x_1)^3$ to $u(x)$ in the remainder.*

## Cheat-sheet
- Double nodes: degree $n=2m+1$; $H_n f=\sum_i[h_{i0}f(x_i)+h_{i1}f'(x_i)]$.
- Hermite basis (double): $h_{i0}=[1-2l_i'(x_i)(x-x_i)]l_i^2$, $h_{i1}=(x-x_i)l_i^2$, with $l_i(x_j)=\delta_{ij}$.
- Degree rule: $\deg = 2\cdot(\#\text{nodes})-1$ (double), or $n+1=\sum_i(r_i+1)$ (general).
- Newton form uses repeated nodes: $f[x_i,x_i]=f'(x_i)$, $f[x_i,x_i,x_i]=\tfrac{f''(x_i)}{2}$, generally $f[\underbrace{x_i,\dots,x_i}_{k+1}]=\tfrac{f^{(k)}(x_i)}{k!}$.
- Double-node error: $R_n(x)=[\psi_m(x)]^2\frac{f^{(n+1)}(\xi_x)}{(n+1)!}$.
- General error: $R_n(x)=\frac{u(x)}{(n+1)!}f^{(n+1)}(\xi_x)$, $u(x)=\prod_i(x-x_i)^{r_i+1}$.
- Cubic Hermite error: $R_3=\frac{(x-a)^2(x-b)^2}{24}f^{(4)}(\xi)$; bound $\frac{(b-a)^4}{384}\max|f^{(4)}|$.
- Special cases: all simple $\to$ Lagrange; one node mult. $n+1\to$ Taylor $T_n f$.
- General fundamental polynomial: $h_{ij}(x)=\frac{(x-x_i)^j}{j!}\big[\sum_{k=0}^{r_i-j}\frac{(x-x_i)^k}{k!}[\frac{1}{u_i(x)}]^{(k)}_{x_i}\big]u_i(x)$, satisfying $h_{ij}^{(k)}(x_i)=\delta_{jk}$.
