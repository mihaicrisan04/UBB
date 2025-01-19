### Cheatsheet

#### Seminar 5: Continuous Random Variables

**1. Continuous Random Variables**
A continuous random variable $X$ has:
• Probability Density Function (PDF) $f(x)$:
$f(x) \geq 0$ for all $x$, and $\int_{-\infty}^\infty f(x) dx = 1$.
• Cumulative Distribution Function (CDF) $F(x)$:

$$
F(x) = P(X \leq x) = \int_{-\infty}^x f(t) dt.
$$

• Probability for an interval:

$$
P(a < X < b) = \int_a^b f(x) dx.
$$

• $P(X = x) = 0$ for any specific value $x$.

**2. Continuous Random Vectors**
For $(X, Y)$, a continuous random vector with joint PDF $f_{X,Y}(x, y)$ and joint CDF $F(x, y)$:
• Joint CDF:

$$
F(x, y) = P(X \leq x, Y \leq y) = \int_{-\infty}^x \int_{-\infty}^y f_{X,Y}(u, v) dv du.
$$

•Marginal PDFs:

$$
f_X(x) = \int_{-\infty}^\infty f_{X,Y}(x, y) dy, \quad f_Y(y) = \int_{-\infty}^\infty f_{X,Y}(x, y) dx.
$$

• Independence:
$X$ and $Y$ are independent if:

$$
f_{X,Y}(x, y) = f_X(x) f_Y(y).
$$

#### Transformation of Random Variables

To find the PDF of a transformed random variable $Y = g(X)$ , where $g$ is a differentiable and strictly monotone function, we use:

$$
f_Y(y) = f_X\left(g^{-1}(y)\right) \cdot \left| \frac{d}{dy} g^{-1}(y) \right|.
$$

Here’s the process:

1. Identify the function $Y = g(X)$.
2. Solve for $X$ in terms of $Y$ to get $X = g^{-1}(Y)$.
3. Compute $\frac{d}{dy} g^{-1}(y)$.
4. Substitute $g^{-1}(y)$ into $f_X(x)$ and multiply by $\left| \frac{d}{dy} g^{-1}(y) \right|$.

---

Exercise Statement

Let $X \sim N(0, 1)$, a standard normal random variable. Find the probability density function (PDF) of $Y = |X|$.

**Step 1: Transformation of a Random Variable**
Theory:
To find the PDF of $Y = g(X)$, we use the following steps when $g(X)$ is not strictly monotonic over the entire domain:

1. Divide the domain of $X$ into monotonic segments.
2. Compute the PDF of $Y$ by summing contributions from all segments.

Here:

$$
Y = |X| =
\begin{cases}
X, & X \geq 0, \\
-X, & X < 0.
\end{cases}
$$

The segments are:
• $X \geq 0$, where $Y = X$.
• $X < 0$, where $Y = -X$.

The corresponding inverse transformations are:
• $X = Y$ for $X \geq 0$,
• $X = -Y$ for $X < 0$.

**Step 2: Derive the PDF of $Y$**
The PDF of $Y$ is obtained by summing over both segments:

$$
f_Y(y) = f_X(y) \cdot \left|\frac{d}{dy} y\right| + f_X(-y) \cdot \left|\frac{d}{dy}(-y)\right|,
$$

where $f_X(x)$ is the standard normal PDF:

$$
f_X(x) = \frac{1}{\sqrt{2\pi}} e^{-x^2 / 2}.
$$

Since $\frac{d}{dy}y = 1$ and $\frac{d}{dy}(-y) = -1$, we have:

$$
f_Y(y) = f_X(y) \cdot 1 + f_X(-y) \cdot 1.
$$


Substitute $f_X(x)$ into the equation:

$$
f_Y(y) = \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2} + \frac{1}{\sqrt{2\pi}} e^{-(-y)^2 / 2}.
$$

Since $(-y)^2 = y^2$:

$$
f_Y(y) = \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2} + \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2}.
$$

Combine the terms:

$$
f_Y(y) = \frac{2}{\sqrt{2\pi}} e^{-y^2 / 2}, \quad y \geq 0.
$$

**Step 3: Domain of $Y$**
Since $Y = |X|$, and $X \sim N(0, 1)$, $Y \geq 0$.

**Result: The PDF of $Y$ is**
$$
f_Y(y) =
\begin{cases}
\frac{2}{\sqrt{2\pi}} e^{-y^2 / 2}, & y \geq 0, \\
0, & y < 0.
\end{cases}
$$

---

#### Seminar 6: Numerical Characteristics of Random Variables



