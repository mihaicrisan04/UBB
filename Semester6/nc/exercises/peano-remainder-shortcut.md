# Quadrature Remainder — The Shortcut

For a quadrature rule with **degree of precision** $d$ and $f \in C^{d+1}[a,b]$:

$$\boxed{\,R(f) = \frac{R(x^{d+1})}{(d+1)!}\, f^{(d+1)}(\xi), \qquad \xi \in (a,b)\,}$$

In words: apply the rule to the **first monomial it gets wrong** ($x^{d+1}$), divide that error by $(d+1)!$, and attach $f^{(d+1)}(\xi)$.

## Why it works

It follows from **Peano's theorem** — for a functional $R$ that vanishes on all polynomials of degree $\le d$:

$$R(f) = \int_a^b K(t)\, f^{(d+1)}(t)\,dt, \qquad K(t) = R_x\!\left[\frac{(x-t)_+^d}{d!}\right].$$

**Step 1 — pull out $f^{(d+1)}$.** *If the kernel $K(t)$ does not change sign* on $[a,b]$, the weighted mean value theorem factors the derivative out at some intermediate $\xi$:
$$R(f) = f^{(d+1)}(\xi)\int_a^b K(t)\,dt.$$

**Step 2 — get $\int K$ for free.** Apply the same identity to $f = x^{d+1}$, whose $(d+1)$-th derivative is the constant $(d+1)!$:
$$R(x^{d+1}) = (d+1)!\int_a^b K(t)\,dt \;\;\Rightarrow\;\; \int_a^b K(t)\,dt = \frac{R(x^{d+1})}{(d+1)!}.$$

Substituting back gives the boxed formula.

## How to use it

1. **Find $d$** — test $1, x, x^2, \dots$; the first that fails fixes $d$ (exact for $1,\dots,x^d$, wrong on $x^{d+1}$).
2. **Compute the error on $x^{d+1}$:** $R(x^{d+1}) = \int_a^b w(x)\,x^{d+1}\,dx - Q(x^{d+1})$, where $Q$ is the quadrature applied to $x^{d+1}$.
3. **Assemble:** $R(f) = \dfrac{R(x^{d+1})}{(d+1)!}\, f^{(d+1)}(\xi)$.

## Computing the kernel $K(t)$ and checking its sign — step by step

The kernel is built from the **cut power function** $\dfrac{(x-t)_+^d}{d!}$, where $(x-t)_+ = \max(x-t, 0)$ is $0$ when $x < t$. Because of that $\max$, $K(t)$ comes out **piecewise in $t$** — that is the "multiple conditions" you have to handle. Procedure:

### Step 1 — apply $R$ to $\dfrac{(x-t)_+^d}{d!}$, term by term

Write the rule as $R(g) = \displaystyle\int_a^b w(x)g(x)\,dx - \sum_i A_i\,g(c_i) - \sum_j B_j\,g'(d_j)$. Holding $t$ fixed, each piece becomes:

- **integral** → $\dfrac{1}{d!}\displaystyle\int_t^b w(x)(x-t)^d\,dx$ (lower limit is $t$, not $a$, since the integrand is $0$ for $x < t$);
- **value node** $c_i$ → $A_i\,\dfrac{(c_i - t)_+^d}{d!}$;
- **derivative node** $d_j$ → $B_j\,\dfrac{(d_j - t)_+^{d-1}}{(d-1)!}$ (each derivative drops the power by one; for $d=1$ this is the step $H(d_j - t)$).

### Step 2 — find the breakpoints

Each $(c_i - t)_+$ and $(d_j - t)_+$ switches off at $t = c_i$ / $t = d_j$. Mark every node lying **inside** $[a,b]$; these cut $[a,b]$ into subintervals. A node at the **left endpoint** is never to the right of an interior $t$, so it contributes nothing — only **interior** nodes create real breakpoints.

### Step 3 — write $K(t)$ on each subinterval

On a given subinterval each $(\text{node} - t)_+$ is unambiguous: **keep** it (drop the $_+$) if the node is to the **right** of $t$; **drop** it (it is $0$) if to the **left**. The integral term $\int_t^b$ is always present. You now have an explicit polynomial for $K(t)$ on each piece.

### Step 4 — check the sign on each piece

On each subinterval $K(t)$ is a polynomial; show it never crosses $0$:

- evaluate $K$ at the subinterval's endpoints (often $0$ at $t=a$ and $t=b$);
- check monotonicity via the sign of $K'(t)$, so it can't dip the other way in between;
- confirm $K$ is **continuous** at each breakpoint (the two adjacent pieces must agree there).

If $K$ keeps the **same sign on every piece** → sign-constant → the shortcut applies. If it changes sign, fall back to $|R(f)| \le \|f^{(d+1)}\|_\infty \int_a^b |K|$.

### Worked example with an interior node ($[0,1]$, $d = 2$)

Rule $R(g) = \int_0^1 g\,dx - g(0) - \tfrac12 g'(1/3)$, so $K(t) = R_x\big[\tfrac{(x-t)_+^2}{2}\big]$.

**Step 1 (term by term).** Using $\tfrac{d}{dx}\tfrac{(x-t)_+^2}{2} = (x-t)_+$:
$$K(t) = \underbrace{\tfrac12\!\int_t^1 (x-t)^2\,dx}_{(1-t)^3/6} \;-\; \underbrace{\tfrac{(0-t)_+^2}{2}}_{=\,0\ (t>0)} \;-\; \tfrac12\,\underbrace{(1/3-t)_+}_{\text{from } g'(1/3)} = \frac{(1-t)^3}{6} - \frac12\,(1/3 - t)_+.$$

**Step 2 (breakpoints).** Node $0$ is the left endpoint → no break. Node $1/3$ is interior → split $[0,1]$ into $[0,\tfrac13)$ and $[\tfrac13,1]$.

**Step 3 (piecewise).**
$$K(t) = \begin{cases} \dfrac{(1-t)^3}{6} - \dfrac12\big(\tfrac13 - t\big), & 0 \le t < \tfrac13 \quad (\tfrac13 \text{ is right of } t \to \text{keep}) \\[2mm] \dfrac{(1-t)^3}{6}, & \tfrac13 \le t \le 1 \quad (\tfrac13 \text{ is left of } t \to \text{drop}) \end{cases}$$

**Step 4 (sign).**
- On $[\tfrac13, 1]$: $\tfrac{(1-t)^3}{6} \ge 0$. ✓
- On $[0, \tfrac13)$: $K(0) = \tfrac16 - \tfrac16 = 0$; $K(\tfrac13) = \tfrac{(2/3)^3}{6} = \tfrac{4}{81} > 0$; $K'(t) = \tfrac12\big[1 - (1-t)^2\big] > 0$ → $K$ rises from $0$, so $K \ge 0$. ✓
- Continuity at $t = \tfrac13$: both pieces give $\tfrac{4}{81}$. ✓

So $K \ge 0$ on all of $[0,1]$ — no sign change → the shortcut is valid, giving $R(f) = \tfrac{R(x^3)}{3!}f'''(\xi) = \tfrac{1}{72}f'''(\xi)$.

**Mnemonic:** for the term at node $c$, ask "is $t$ left or right of $c$?" — right of $c$ → keep it; left of $c$ → it is $0$. Left-endpoint nodes are always "to the left," so they vanish.

## Examples (from the worked exercises)

| rule | $d$ | $R(x^{d+1})$ | $R(f)$ |
|---|---|---|---|
| weighted $w=x^2$ on $[-1,1]$ | 1 | $\tfrac25$ | $\tfrac{2/5}{2!}f'' = \tfrac15 f''(\xi)$ |
| Birkhoff on $[-1,1]$ | 1 | $-\tfrac{16}{3}$ | $\tfrac{-16/3}{2!}f'' = -\tfrac83 f''(\xi)$ |
| max-DoP on $[0,1]$ | 2 | $\tfrac{1}{12}$ | $\tfrac{1/12}{3!}f''' = \tfrac{1}{72}f'''(\xi)$ |

## Caveat

The shortcut needs the **Peano kernel to keep its sign** (that justifies Step 1). For the simple one-node rules here it always does, but a rigorous answer still verifies it. If $K$ changed sign you could not pull out $f^{(d+1)}(\xi)$ — you would instead use the bound $|R(f)| \le \|f^{(d+1)}\|_\infty \int_a^b |K(t)|\,dt$. The shortcut and the full Peano computation give the **same** constant; the shortcut just skips building $K$.
