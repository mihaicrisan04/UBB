### Cheatsheet

#### Seminar 5: Continuous Random Variables

**1. Continuous Random Variables**
A continuous random variable $X$ has:
• Probability Density Function (PDF) $f(x)$:
$f(x) \geq 0$ for all $x$, and $\int_{-\infty}^\infty f(x) dx = 1$.
• Cumulative Distribution Function (CDF) $F(x)$:

$
F(x) = P(X \leq x) = \int_{-\infty}^x f(t) dt.
$

• Probability for an interval:

$
P(a < X < b) = \int_a^b f(x) dx.
$

• $P(X = x) = 0$ for any specific value $x$.

---

**2. Continuous Random Vectors**
For $(X, Y)$, a continuous random vector with joint PDF $f_{X,Y}(x, y)$ and joint CDF $F(x, y)$:
• Joint CDF:

$
F(x, y) = P(X \leq x, Y \leq y) = \int_{-\infty}^x \int_{-\infty}^y f_{X,Y}(u, v) dv du.
$

• Marginal PDFs:

$
f_X(x) = \int_{-\infty}^\infty f_{X,Y}(x, y) dy, \quad f_Y(y) = \int_{-\infty}^\infty f_{X,Y}(x, y) dx.
$

• Independence:
$X$ and $Y$ are independent if:

$
f_{X,Y}(x, y) = f_X(x) f_Y(y).
$

---

**Transformation of Random Variables**
To find the PDF of a transformed random variable $Y = g(X)$ , where $g$ is a differentiable and strictly monotone function, we use:

$
f_Y(y) = f_X\left(g^{-1}(y)\right) \cdot \left| \frac{d}{dy} g^{-1}(y) \right|.
$

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

$
Y = |X| =
\begin{cases}
X, & X \geq 0, \\
-X, & X < 0.
\end{cases}
$

The segments are:
• $X \geq 0$, where $Y = X$.
• $X < 0$, where $Y = -X$.

The corresponding inverse transformations are:
• $X = Y$ for $X \geq 0$,
• $X = -Y$ for $X < 0$.

**Step 2: Derive the PDF of $Y$**
The PDF of $Y$ is obtained by summing over both segments:

$
f_Y(y) = f_X(y) \cdot \left|\frac{d}{dy} y\right| + f_X(-y) \cdot \left|\frac{d}{dy}(-y)\right|,
$

where $f_X(x)$ is the standard normal PDF:

$
f_X(x) = \frac{1}{\sqrt{2\pi}} e^{-x^2 / 2}.
$

Since $\frac{d}{dy}y = 1$ and $\frac{d}{dy}(-y) = -1$, we have:

$
f_Y(y) = f_X(y) \cdot 1 + f_X(-y) \cdot 1.
$


Substitute $f_X(x)$ into the equation:

$
f_Y(y) = \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2} + \frac{1}{\sqrt{2\pi}} e^{-(-y)^2 / 2}.
$

Since $(-y)^2 = y^2$:

$
f_Y(y) = \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2} + \frac{1}{\sqrt{2\pi}} e^{-y^2 / 2}.
$

Combine the terms:

$
f_Y(y) = \frac{2}{\sqrt{2\pi}} e^{-y^2 / 2}, \quad y \geq 0.
$

**Step 3: Domain of $Y$**
Since $Y = |X|$, and $X \sim N(0, 1)$, $Y \geq 0$.

**Result: The PDF of $Y$ is**
$
f_Y(y) =
\begin{cases}
\frac{2}{\sqrt{2\pi}} e^{-y^2 / 2}, & y \geq 0, \\
0, & y < 0.
\end{cases}
$

---

#### Seminar 6: Numerical Characteristics of Random Variables



#### Seminar 7: Inequalities, Central Limit Theorem, Point Estimators

**1. Markov’s Inequality**
For any non-negative random variable $X$ and any $a > 0$:

$
P(|X| \geq a) \leq \frac{E(|X|)}{a}
$

---

**2. Chebyshev’s Inequality**
For any random variable $X$ with finite expectation $E(X)$ and variance $V(X)$, and for any $\varepsilon > 0$:

$
P(|X - E(X)| \geq \varepsilon) \leq \frac{V(X)}{\varepsilon^2}
$

---

**3. Central Limit Theorem (CLT)**
Let $X_1, X_2, \dots, X_n$ be independent and identically distributed (i.i.d.) random variables with mean $\mu = E(X_i)$ and standard deviation $\sigma$. Define the sum:

$
S_n = \sum_{i=1}^{n} X_i
$

As $n \to \infty$, the standardized sum:

$
Z_n = \frac{S_n - E(S_n)}{\sigma(S_n)} = \frac{S_n - n\mu}{\sigma \sqrt{n}}
$

converges in distribution to the standard normal distribution:

$
Z_n \to N(0,1)
$

This means the cumulative distribution function (CDF) of $Z_n$ approaches the standard normal CDF $\Phi(z)$.

---

**Steps for Solving Problems Using Methods of Moements (MoM)**
**Step 1: Find theoretical moments**

• Compute the theoretical mean (first moment), variance (second moment), or higher moments from the given probability density function (PDF) or cumulative distribution function (CDF).
• Example:

$
E(X) = \int_{-\infty}^{\infty} x f(x; \theta) \, dx
$

**Step 2: Compute sample moments**
Calculate the sample moments from the given data:

$
\bar{X} = \frac{1}{n} \sum_{i=1}^{n} X_i
$

**Step 3: Solve for the parameter(s)**
• Set the sample moments equal to the theoretical moments:

$
\text{Theoretical Moment} = \text{Sample Moment}
$

$
E(X) = \bar{X}
$

$
\int_{-\infty}^{\infty} x f(x; \theta) \, dx = \frac{1}{n} \sum_{i=1}^{n} X_i
$

• Solve the resulting equation for the unknown parameter(s).

---

**Steps for Solving Problems Using MLE**
**Step 1: Write the likelihood function**
• Given the PDF of the distribution, construct the likelihood function by multiplying the PDFs of the sample data:

$
L(\theta) = \prod_{i=1}^{n} f(X_i; \theta)
$

$
L(\theta) = f(X_1; \theta) \cdot f(X_2; \theta) \cdot \dots \cdot f(X_n; \theta)
$

**Step 2: Take the natural logarithm (log-likelihood function)**
• The log-likelihood function simplifies calculations:

$
\ln L(\theta) = \sum_{i=1}^{n} \ln f(X_i; \theta)
$

**Step 3: Differentiate the log-likelihood with respect to $\theta$**
• Compute the derivative of the log-likelihood with respect to the unknown parameter:

$
\frac{d}{d\theta} \ln L(\theta) = 0
$

**Step 4: Solve for the parameter**
• Solve the resulting equation to find the maximum likelihood estimator $\hat{\theta}$.

---

**Standard Error of an Estimator:**
The standard error of an estimator $\hat{\theta}$ is:

$
\sigma_{\hat{\theta}} = \sqrt{V(\hat{\theta})}
$

---

**Fisher Information:**
$
I_n(\theta) = -E\left[\frac{\partial^2 \ln L(X_1, \dots, X_n; \theta)}{\partial \theta^2}\right]
$

If the range of $X$ does not depend on $\theta$, then:

$
I_n(\theta) = n I_1(\theta)
$

---

**Efficiency of an Estimator:**
The efficiency of an estimator $\hat{\theta}$ is:

$
e(\theta) = \frac{1}{I_n(\theta)V(\hat{\theta})}
$

---

**Estimator Properties:**

An estimator $\hat{\theta}$ of the parameter $\theta$ is:

- Unbiased: if $E(\hat{\theta}) = \theta$.
- Absolutely Correct: if $E(\hat{\theta}) = \theta$ and $V(\hat{\theta}) \to 0$ as $n \to \infty$.
- MVUE (Minimum Variance Unbiased Estimator): if $E(\hat{\theta}) = \theta$ and its variance is the lowest among all unbiased estimators.
- Efficient: if $e(\theta) = 1$, meaning it achieves the Cramér-Rao lower bound.

Note: If an estimator is efficient, it is also the MVUE.
