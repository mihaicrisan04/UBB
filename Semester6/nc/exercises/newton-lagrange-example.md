# Newton / Lagrange Interpolation — Worked Example

**Problem.** Find the minimum-degree polynomial that passes through the three points

$$(0, 1), \quad (1, 3), \quad (2, 4),$$

write it in Newton form, evaluate it at $x = 1.5$, and cross-check with the Lagrange form.

## Choosing the method

The nodes $0, 1, 2$ are **distinct** and we are given **only function values** (no derivatives), so this is ordinary (simple-node) interpolation — use Newton's divided-difference table. Each node appears **once**, so there are no duplicates and every entry is an ordinary quotient (no derivative seeding, unlike Hermite).

Number of conditions $= 3 \Rightarrow$ degree $n = 3 - 1 = 2$ (a quadratic).

## Step 1 — list the nodes (each once)

$$x_0 = 0, \quad x_1 = 1, \quad x_2 = 2.$$

## Step 2 — build the divided-difference table

The recurrence is

$$f[x_i, \dots, x_{i+k}] = \frac{f[x_{i+1}, \dots, x_{i+k}] - f[x_i, \dots, x_{i+k-1}]}{x_{i+k} - x_i}.$$

First divided differences:

$$f[x_0, x_1] = \frac{3 - 1}{1 - 0} = 2, \qquad f[x_1, x_2] = \frac{4 - 3}{2 - 1} = 1.$$

Second divided difference:

$$f[x_0, x_1, x_2] = \frac{f[x_1, x_2] - f[x_0, x_1]}{x_2 - x_0} = \frac{1 - 2}{2 - 0} = -\frac{1}{2}.$$

The full table (coefficients are the **top entry of each column**):

$$
\begin{array}{c|ccc}
x & f & \text{1st DD} & \text{2nd DD}\\
\hline
0 & 1 & & \\
  &   & f[x_0,x_1] = 2 & \\
1 & 3 & & f[x_0,x_1,x_2] = -\tfrac{1}{2}\\
  &   & f[x_1,x_2] = 1 & \\
2 & 4 & &
\end{array}
$$

## Step 3 — read off the coefficients

$$c_0 = 1, \qquad c_1 = 2, \qquad c_2 = -\frac{1}{2}.$$

## Step 4 — assemble the Newton form

The product terms follow the node order $0, 1, 2$:

$$N_2 f(x) = c_0 + c_1 (x - x_0) + c_2 (x - x_0)(x - x_1) = 1 + 2(x - 0) - \frac{1}{2}(x - 0)(x - 1).$$

Expanding:

$$N_2 f(x) = 1 + 2x - \frac{1}{2}\big(x^2 - x\big) = -\frac{1}{2}x^2 + \frac{5}{2}x + 1.$$

## Step 5 — verify it passes through the points

- $N_2(0) = 1$ ✓
- $N_2(1) = -\tfrac{1}{2} + \tfrac{5}{2} + 1 = 3$ ✓
- $N_2(2) = -2 + 5 + 1 = 4$ ✓

## Step 6 — evaluate at $x = 1.5$

$$N_2(1.5) = -\frac{1}{2}(1.5)^2 + \frac{5}{2}(1.5) + 1 = -1.125 + 3.75 + 1 = 3.625.$$

## Cross-check — the Lagrange form (same polynomial)

The Lagrange basis polynomials for nodes $0, 1, 2$ are

$$\ell_0(x) = \frac{(x-1)(x-2)}{(0-1)(0-2)} = \frac{(x-1)(x-2)}{2},$$
$$\ell_1(x) = \frac{(x-0)(x-2)}{(1-0)(1-2)} = -x(x-2),$$
$$\ell_2(x) = \frac{(x-0)(x-1)}{(2-0)(2-1)} = \frac{x(x-1)}{2}.$$

So

$$L_2 f(x) = 1\cdot\ell_0(x) + 3\cdot\ell_1(x) + 4\cdot\ell_2(x).$$

At $x = 1.5$: $\ell_0 = \tfrac{(0.5)(-0.5)}{2} = -\tfrac{1}{8}$, $\ell_1 = -(1.5)(-0.5) = \tfrac{3}{4}$, $\ell_2 = \tfrac{(1.5)(0.5)}{2} = \tfrac{3}{8}$, giving

$$L_2 f(1.5) = 1\cdot\left(-\tfrac{1}{8}\right) + 3\cdot\tfrac{3}{4} + 4\cdot\tfrac{3}{8} = -\tfrac{1}{8} + \tfrac{9}{4} + \tfrac{3}{2} = \tfrac{29}{8} = 3.625.$$

Same value — Newton and Lagrange are just two writings of the **same unique** interpolating polynomial.

## Remainder (if the data came from a known $f \in C^3$)

If the points are samples of a function $f$, the interpolation error at a point $x$ is

$$R_2 f(x) = \frac{(x - x_0)(x - x_1)(x - x_2)}{3!}\,f'''(\xi), \qquad \xi \in (0, 2).$$

At $x = 1.5$ the node-polynomial factor is $\dfrac{(1.5)(0.5)(-0.5)}{6} = -\dfrac{1}{16}$, so $|R_2 f(1.5)| = \dfrac{1}{16}\,|f'''(\xi)| \le \dfrac{1}{16}\max_{[0,2]}|f'''|$.

## Key takeaway

The simple-node table is just Newton's interpolation: first column is the $f$ values, every other entry is an ordinary difference quotient, and the coefficients are the top of each column. Because each node appears once, there is **no** derivative seeding — that step only arises in Hermite, where a node is repeated. Lagrange gives the identical polynomial in a non-incremental, basis-function form.
