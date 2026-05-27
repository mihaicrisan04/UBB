# Hermite Interpolation — Worked Example

**Problem.** Given $f(0)$, $f'(0)$, and $f(1)$, build the Hermite interpolating polynomial.

We have **3 conditions** ($f(0)$, $f'(0)$, $f(1)$), so the interpolant has degree $n = 3 - 1 = 2$ (a quadratic).

## Step 1 — list the nodes with repetition

Write each node as many times as you have information about it:

- node $0$: you know $f(0)$ **and** $f'(0)$ → list it **twice**
- node $1$: you only know $f(1)$ → list it **once**

So the ordered node list is

$$z_0 = 0, \quad z_1 = 0, \quad z_2 = 1.$$

The product terms in the Newton form follow this exact order:

$$1, \quad (x - z_0), \quad (x - z_0)(x - z_1), \ \dots$$

## Step 2 — first column = the $f$ values

| $z$ | $f$ |
|-----|------|
| $0$ | $f(0)$ |
| $0$ | $f(0)$ |
| $1$ | $f(1)$ |

## Step 3 — fill in the divided differences (the "seeds")

The recurrence between consecutive entries is

$$f[z_i, \dots, z_{i+k}] = \frac{f[z_{i+1}, \dots, z_{i+k}] - f[z_i, \dots, z_{i+k-1}]}{z_{i+k} - z_i}.$$

**But** when the two endpoints are the *same node*, the denominator is $0$, so you cannot use the quotient. Instead you **seed** that entry with the derivative:

$$f[0,0] = f'(0) \qquad \text{(the starting term for the repeated node).}$$

In general, a node repeated $k+1$ times is seeded with

$$f[\underbrace{x_i, \dots, x_i}_{k+1}] = \frac{f^{(k)}(x_i)}{k!}.$$

Everything else uses the normal quotient:

$$f[0,1] = \frac{f(1) - f(0)}{1 - 0} = f(1) - f(0),$$

$$f[0,0,1] = \frac{f[0,1] - f[0,0]}{1 - 0} = f(1) - f(0) - f'(0).$$

The full table (coefficients are the **top entry of each column**):

$$
\begin{array}{c|cccc}
z & f & \text{1st DD} & \text{2nd DD} \\
\hline
0 & f(0) & & \\
  &      & f[0,0] = f'(0) & \\
0 & f(0) & & f[0,0,1] \\
  &      & f[0,1] = f(1) - f(0) & \\
1 & f(1) & &
\end{array}
$$

## Step 4 — read off the coefficients

$$c_0 = f(0), \qquad c_1 = f'(0), \qquad c_2 = f(1) - f(0) - f'(0).$$

## Step 5 — assemble the polynomial

The product terms use the listed node order $0, 0, 1$:

$$H_2 f(x) = \underbrace{f(0)}_{c_0} + \underbrace{f'(0)}_{c_1}\,(x - 0) + \underbrace{\big(f(1) - f(0) - f'(0)\big)}_{c_2}\,(x - 0)(x - 0).$$

$$\boxed{\,H_2 f(x) = f(0) + f'(0)\,x + \big(f(1) - f(0) - f'(0)\big)\,x^2\,}$$

## Step 6 — verify all three conditions

- $H_2(0) = f(0)$ ✓
- $H_2'(x) = f'(0) + 2\big(f(1) - f(0) - f'(0)\big)x \ \Rightarrow\ H_2'(0) = f'(0)$ ✓
- $H_2(1) = f(0) + f'(0) + f(1) - f(0) - f'(0) = f(1)$ ✓

## Key takeaway

The table starts exactly like Newton's interpolation — the first column is the $f$ values. The **only** twist for Hermite is that any bracket whose arguments are *all the same node* is *seeded* with a derivative instead of an undefined $\tfrac{0}{0}$ quotient. A node listed $k+1$ times produces $k$ such brackets, and you seed **each** of them:

$$f[x,x] = f'(x), \quad f[x,x,x] = \frac{f''(x)}{2!}, \quad \dots, \quad f[\underbrace{x, \dots, x}_{k+1}] = \frac{f^{(k)}(x)}{k!}.$$

In this example node $0$ had multiplicity only $2$, so there was a single such bracket, $f[0,0] = f'(0)$. A node of multiplicity $3$ would have two seeds, $f[x,x] = f'(x)$ **and** $f[x,x,x] = \tfrac{f''(x)}{2}$. Mixed brackets such as $f[0,0,1]$ always use the normal quotient.

### Why degree $=$ (number of conditions) $- 1$

A degree-$n$ polynomial has $n+1$ free coefficients, so it takes exactly $n+1$ independent conditions to determine it uniquely. The number of conditions equals the total information, i.e. the sum of the node multiplicities $\sum_i (r_i + 1)$. Here node $0$ contributes $2$ ($f(0), f'(0)$) and node $1$ contributes $1$ ($f(1)$), giving $3$ conditions and degree $3 - 1 = 2$.
