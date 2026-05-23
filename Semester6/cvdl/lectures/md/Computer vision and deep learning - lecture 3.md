# Computer Vision and Deep Learning

Lecture 3

# Today’s agenda

- Training a neural network
- Computational graphs
- Optimization algorithms
- Initialization techniques
- Learning rate schedules
- Bias and variance
- Hyper-parameter tuning
- Regularization techniques

	

- 

# Gradient descent

- The core optimization algorithm behind training neural networks
- Computes the gradient of the loss w.r.t. all parameters, then steps in the opposite direction
- Repeats until convergence (or a set number of epochs)

# Gradient descent

θ – weights, η – learning rate, x – input, y – ground truth, ŷ – prediction, L – loss 

INITIALIZE weights θ

FOR each epoch:

    FOR each mini-batch:

        // Forward pass

        compute predictions ŷ = f(X, θ)

        compute loss L(ŷ, y)

        // Backward pass

        compute gradients ∇L(θ) via backprop

        // Update

        θ = θ − η · ∇L(θ)

# How do we compute the gradients?

- Numerical gradient
- Analytical gradient
- Better idea: use computational graphs and back-propagation 

<!-- image -->

# Computational graphs

f(a, b, c) = a*b + c

*

+

a

b

c

f

# Back propagation

f

x

y

z

Local gradients

- Influence of x and y on the nodes output z

# Back propagation

f

x

y

z

Influence of z to the final output of the graph (loss)

# Back propagation

Use a recursive application of the chain rule on every node in the graph to computer influence of all the intermediate nodes on the output of the graph

# Computational graphs – forward pass

f(a, b, c) = a*b + c

a = 3

b = 2

c = -4

*

+

a

b

c

f

3

4

-4

12

8

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

1

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

x

x = a*b

f = x + c

1

1

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

f(a, b, c) = a*b + c

*

+

a

b

c

f

3

4

-4

12

8

x

x = a*b

f = x + c

1

1

# Computational graphs – backward pass

Go backward, from the end of the computational graph, and compute the gradient for each node in the graph

# Backprop example with a neural network in mind

In a neural network a neuron computes a linear function, followed by a non-linearity (activation function).

<!-- image -->

x0

x1

z = w0*x0+ w1*x1 + w2

a = σ(z) 

w0

w1

w2

1

# Backprop example With a neural network in mind

In a neural network a neuron computes a linear function, followed by a non-linearity (activation function).

<!-- image -->

x0

x1

z = w0*x0+ w1*x1 + w2

a = σ(z) 

w0

w1

w2

1

Let’s write this basic neuron as a computational graph!

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

a = σ(z) 

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

sigmoid

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

σ(1)  = 0.7310585786300048792512

a = σ(z) 

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

???

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

1*  (-1/1.372)

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

???

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53*1

???

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.53*exp(-1)

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.2

???

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.2

-0.2*(-1)

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.2

0.2

1*0.2

1*0.2

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.2

0.2

0.2

0.2

0.2

0.2

0.2* (-1)

0.2* 2

# Backprop example With a neural network in mind

x0

x1

z = w0*x0+ w1*x1 + w2

w0

w1

w2

1

Example credit: https://cs231n.github.io/optimization-2/ , figure 2 

*

*

+

+

*-1

+1

exp

1/x

w0

x0

w1

x1

w2

2

-1

-3

-2

-3

-2

6

4

-1

1

0.37

1.37

0.73

a = σ(z) 

1

 -0.53

-0.53

-0.2

0.2

0.2

0.2

0.2

0.2

-0.2

0.4

-0.4

-0.6

# Multivariate chain rule

+

# Patterns in backpropagation

- Addition gate: gradient distributor
- Multiplication gate: gradient switcher
- Max gate: gradient router
- Copy gate: gradient adder

*

*

+

max

a

b

c

d

e

# Back-propagation for vectors

<!-- image -->

# Forward/backward API

<!-- image -->

class ComputationalGraph():

   def forward(inputs):         for node in topological\_sort(self.nodes):             # forward inputs through each node             # (also cache these inputs)             node.forward()         return predictions

        def backward(loss):         for node in reversed(topological\_sort(self.nodes)):             # backpropagation (apply chain rule)             node.backward()         return input\_gradients

# Some prerequisites

Exponential weighted averages

# Exponential weighted averages

https://towardsdatascience.com/stochastic-gradient-descent-with-momentum-a84097641a5d 

<!-- image -->

https://colab.research.google.com/drive/1CTSctxtN1JDvGZOswpLlG9ZkC4-s0ym2?usp=sharing 

# Exponential weighted averages

<!-- image -->

Vt approximates over: 

samples

# Exponential weighted average

<!-- image -->

<!-- image -->

<!-- image -->

The coefficients add up to approximately 1

# Exponential weighted average

<!-- image -->

Forgetting: threshold 1/e

(1 – ε)1/ ε = 1/e

https://www.youtube.com/watch?v=lAq96T8FkTw intuition for approximation 

1/e

# Exponential weighted average Bias correction

https://www.youtube.com/watch?v=lAq96T8FkTw 

https://www.youtube.com/watch?v=NxTFlzBjS-4 

https://www.youtube.com/watch?v=lWzo8CajF5s – bias correction

# Training a neural network

Optimization algorithms

# Parameters update – gradient descent

Vanilla gradient descent:

1. Forward propagation through the network
2. Compute loss
3. Back-propagate to compute gradients
4. Update the parameters using the gradient

# Size of the mini-batches

- Stochastic gradient descent (bs=1)
- Mini-batch gradient descent 
- Batch gradient descent (bs = N)

<!-- image -->

# Size of the mini-batches

- Stochastic gradient descent (bs=1)
- no vectorization, lose speedup
- Mini-batch gradient descent 
- Fastest learning: use vectorization and doesn’t take top much time to update the weights
- 26 , 27 , …, 210
- Batch gradient descent (m = N)
- Too much time per iteration

# Level sets of a surface

<!-- image -->

https://mathinsight.org/level\_sets 

# Problems with vanilla gradient descent

<!-- image -->

What if loss changes abruptly on one direction and slower in the other direction? 

# Problems with vanilla gradient descent

What if loss changes abruptly on one direction and slower in the other direction? 

<!-- image -->

# Problems with gradient descent

What if loss changes abruptly on one direction and slower in the other direction? 

- Small gradient horizontally 
- Large gradient vertically 
-  

<!-- image -->

Slow progress along the horizontal direction, jitter along the vertical direction (the steeper one)

# Gradient descent with momentum

Idea: Compute an exponentially weighted average of the gradients and use this average to update the parameters of the network

# Gradient descent with momentum Update rule

# Gradient descent with momentum

Take more straightforward part (damp oscillations) 

 

Slower learning vertically, faster learning horizontally

# Gradient descent with momentum

- Intuition
- Ball rolling down the loss function with friction
- Gradient: the force the ball is feeling (F = ma)

velocity

acceleration

Shallow, consistent direction: build up the velocity across the dimension

Steep direction: attenuate velocity (quickly changing sign); oscillate to the “middle”

# Gradient descent with momentum Update rule - variant

# Nesterov momentum

https://dominikschmidt.xyz/nesterov-momentum/ 

<!-- image -->

Image source: https://cs231n.github.io/neural-networks-3/#sgd 

<!-- image -->

<!-- image -->

# AdaGrad

# AdaGrad

<!-- image -->

Equalizing effect

- Larger learning rate on “shallow” directions than on steeper directions

# RMSProp

<!-- image -->

Introduced by Geoffrey Hinton on a lecture on Coursera:

Adagrad

RMSProp

# Adam

momentum

(velocity)

RMSProp like

RMSProp

# Adam

momentum

(velocity)

RMSProp like

RMSProp

Noisy gradients

# Adam

Bias correction

http://www.denizyuret.com/2015/03/alec-radfords-animations-for.html 

<!-- image -->

# Optimization algorithms

https://awesomeopensource.com/project/3springs/viz\_torch\_optim 

# AdamW

- L2 regularization is the go to regularization technique
- add ½ꞏλ‖θ‖² to the loss
- λ regularization strength
- θ – learned weights
-  This adds λ ꞏ θ to the gradient, which shrinks weights toward zero at each step
- dW←dW+λW 
- SGD: L2 regularization works exactly as intended
- ADAM: ?

# AdamW

# AdamW - The Fix

- Decouple weight decay from the gradient update
- Instead of folding regularization into the gradient, subtract the weight penalty directly and separately after the Adam step

# AdamW

# AdamW

- Achieves better generalization than Adam + L2 on image and language tasks
- You can tune the regularization strength independently of the learning rate
- Is now the default optimizer for nearly all large language models (GPT, BERT, LLaMA all use AdamW)

https://arxiv.org/abs/1711.05101 

# Lion optimizer

https://arxiv.org/pdf/2302.06675 

# Lion optimizer

- Memory: Only needs one momentum buffer m, not two (m and v) like Adam. 33% less optimizer memory (just W and m, no v).
- Every parameter gets the same size step: no adaptive scaling per parameter. This acts as a form of implicit normalization.
- Effective learning rate: Because all steps are +1/-1, Lion typically needs a smaller learning rate (~3-10x smaller than Adam) and larger weight decay
- 

# Training a neural network

Learning rate scheduling

# Learning rate

- Learning rate: scalar that controls how big each update step is
- It is one of the most important hyperparameters in training (tune it first)
- Does not affect the direction of the update, only the magnitude

# Learning rate

- A fixed learning rate is rarely optimal throughout training
- Early training: large LR helps escape bad initializations and explore the loss landscape quickly
- Late training: small LR needed to settle into a minimum precisely without overshooting
- Schedules give you fast early progress and later fine-grained convergence

# Step decay

- η = η₀ · γ^(floor(epoch / step\_size))
- Reduces LR by a factor γ every fixed number of epochs (e.g. multiply by 0.1 every 30 epochs)
- Loss often visibly improves right after each drop
- Used in classic CNN training (ResNet, VGG)
- Drawback: Requires manual tuning of when to drop and by how much

<!-- image -->

# Exponential Decay 

- η = η₀ · e^(−λt)
- Smoothly and continuously reduces LR at every step
- Simpler to reason about than step decay - one hyperparameter λ controls the decay rate
- Drawback: LR decays even in early training when a larger LR would still be required

<!-- image -->

# Cosine Annealing 

- η = η\_min + 0.5 · (η\_max − η\_min) · (1 + cos(πt / T))
- Smoothly decays LR following a cosine curve from η\_max to η\_min over T steps. This avoids the abrupt loss jumps of step decay
- Used in modern vision training (ViT, ResNet, EfficientNet)
- Drawback: Need to set T (total steps) in advance
- 

# Cosine Annealing 

- η = η\_min + 0.5 · (η\_max − η\_min) · (1 + cos(πt / T))
-  η\_max  = 0.1  η\_min   = 1e-4

     epochs = 100

- 

<!-- image -->

# Cosine Annealing with Warm Restars

- Cosine annealing but the LR periodically resets back to η\_max
- Each restart forces the model to escape the current minimum and explore again
- Can find more generalizable minima across multiple cycles
- Period can be fixed or grow longer with each restart (common pattern)
- Drawback: Restarts can temporarily hurt performance right after the LR spikes back up

# Cosine Annealing with Warm Restars

- η\_t = η\_min + 0.5 · (η\_max − η\_min) · (1 + cos(π · (t mod T₀) / T₀)) 
- t: current epoch. T₀: length of the first cycle (# of epochs before first restart). t mod T₀: resets t back to 0 at the start of each cycle

<!-- image -->

# Linear Warmup 

- η = η\_max · (t / T\_warmup) if t &lt; T\_warmup else η\_max
- Starts from a very small LR and linearly increases to η\_max over T\_warmup steps
- A large LR at init (weights are random and gradients are noisy) can cause destructive updates
- Warmup gives the model time to stabilize before applying full learning rate
- Typically combined with cosine annealing after warmup ends
- Almost always used in transformer training (ViT, Swin, DINO)

# Linear warmup

# Warmup &amp; Cosine Decay

- The most common schedule in current vision models training
- Phase 1: linear warmup for a few epochs (e.g. 5-20)
- Phase 2: cosine decay for the remainder of training
- Used in Swin Transformer, DINO, MAE, and most recent vision models
- Balances stable early training with smooth late convergence

# Activation functions

# Why do we need activation functions?

Layer 1:

	Z1 = W1·X

Layer 2:

	Z2 = W2·g(W1·X), where g is the activation function

What happens if we didn’t use a non-linearity?

X

Linear unit

W1

Non-linearity

Linear unit

W2

Non-linearity

scores

# Activation functions

<!-- image -->

Two properties:

- Differentiable
- Non-linear
- 

Commonly used activation functions and their derivative 

https://ml-cheatsheet.readthedocs.io/en/latest/activation\_functions.html

# Rectified Linear Unit (ReLU)

- Most widely used activation in CNNs
- Computationally cheap 
- Solves the vanishing gradient problem for positive values
- Drawback: "Dying ReLU" - neurons can get stuck outputting 0 permanently

<!-- image -->

<!-- image -->

# Leaky ReLU

- Fixes the dying ReLU problem by allowing a small negative slope
- Keeps gradients flowing even for negative inputs
- α is a small hyperparameter (0.01)
- Used in GANs and detection models like YOLO

<!-- image -->

<!-- image -->

# Exponential Linear Unit (ELU)

- Smooth for negative values → faster convergence
- Mean activations closer to zero → less internal covariate shift
- More computationally expensive than ReLU 
- Performs well in deep architectures

<!-- image -->

# Gaussian Error Linear Unit (GeLU)

- f(x) = x · Φ(x), Φ(x) is the CDF of the standard normal distribution
- Smooth, probabilistic interpretation
- Outperforms ReLU on many vision benchmarks
- Slightly more expensive but preferred in vision transformers

<!-- image -->

<!-- image -->

# GELU

- Φ(x) is the CDF of the standard normal distribution. i.e. the probability that a Gaussian random variable is ≤ x
- ReLU – binary decision : is x &gt; 0? If yes, pass it. If no, supress it.
- GELU - soft, probabilistic decision: how likely is this value to be positive? Then it scales x by that probability.
- If x is “very” positive → Φ(x) ≈ 1 → output ≈ x (passes through fully)
- If x is “very” negative → Φ(x) ≈ 0 → output ≈ 0 (mostly suppressed)
- If x is near 0 → Φ(x) ≈ 0.5 → output ≈ 0.5x (partially passed)
- 

# Swish

- f(x) = x · sigmoid(x), smooth and self-gated
- Used in EfficientNet, MobileNetV3
- Outperforms ReLU on deeper networks
- Slightly more compute than ReLU but often worth the tradeoff

<!-- image -->

<!-- image -->

# Sigmoid

- Squashes any input into a value between 0 and 1
- Not used in hidden layers of deep CNNs (vanishing gradient problem)
- Still common in attention gates and final layers for multi-label tasks
- Used in sigmoid attention (e.g., SigLIP)
- 
- 
- 
- 
- 

<!-- image -->

<!-- image -->

# Tanh

- Outputs in range (−1, 1) and is zero-centered, unlike sigmoid
- Was the dominant activation before ReLU took over
- Still used in RNNs/LSTMs, and occasionally in vision-language models

<!-- image -->

<!-- image -->

# Initialization techniques

# Weight initialization

- At the start of training, weights are random; how random matters!
- Too large: activations explode as they pass through layers → gradients explode → training diverges.
- Too small: activations shrink to zero as they pass through layers → gradients vanish → no learning

<!-- image -->

Image source: https://www.coursera.org/learn/deep-neural-network/lecture/C9iQO/vanishing-exploding-gradients 

# How NOT to initialize: zero weights

<!-- image -->

<!-- image -->

- Naively setting all weights to zero seems safe but completely breaks training
- Every neuron computes the same output, produces the same gradient, and updates identically 
- This is called the symmetry problem: if all weights are equal, they stay equal forever
- You can initialize biases to 0

# Random initialization

- W ~ U(−0.01, 0.01) or W ~ N(0, 0.01²)
- Simple fix for symmetry: weights are different so neurons can specialize
- Works for very shallow networks but fails in deep ones
- With many layers, even small variances compound: activations either explode or vanish depending on the scale chosen

<!-- image -->

input layer had mean -0.001449 and stddev 0.999223

hidden layer 0 had mean 0.000289 and stddev 0.213667

hidden layer 1 had mean -0.000064 and stddev 0.047754

hidden layer 2 had mean -0.000001 and stddev 0.010667

hidden layer 3 had mean -0.000003 and stddev 0.002391

hidden layer 4 had mean 0.000000 and stddev 0.000535

hidden layer 5 had mean -0.000000 and stddev 0.000119

hidden layer 6 had mean 0.000000 and stddev 0.000027

hidden layer 7 had mean -0.000000 and stddev 0.000006

hidden layer 8 had mean 0.000000 and stddev 0.000001

hidden layer 9 had mean 0.000000 and stddev 0.000000

W = np.random.randn(in, out)*0.01

<!-- image -->

https://colab.research.google.com/drive/1CvCXZIevs6MvQnldyG2UHZevG0bKaeiu?usp=sharing  Example adapted after Andrew Karpathy’s lecture

# Xavier / Glorot initialization

- Designed for tanh / sigmoid
- Simplified form: W ~ N(0, 1/fan\_in) 
- N(m, v) – normal distribution with mean m and variance v
- Full symmetric form (Glorot &amp; Bengio, 2010): W ~ N(0, 2/(fan\_in + fan\_out)) 
- Idea: keep Var(output) ≈ Var(input) at each layer
- Linear layer with n inputs, the variance of the pre-activation is n · Var(W) · Var(x)
- Setting Var(W) = 1/n stabilizes the signal
- Works well with 0-centered activations

https://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf 

# Xavier initialization

<!-- image -->

hidden layer 0 had mean -0.000145 and stddev 0.628485

hidden layer 1 had mean 0.000485 and stddev 0.486178

hidden layer 2 had mean -0.000333 and stddev 0.408851

hidden layer 3 had mean -0.000013 and stddev 0.358011

hidden layer 4 had mean -0.000238 and stddev 0.323565

hidden layer 5 had mean 0.000176 and stddev 0.295040

hidden layer 6 had mean -0.000547 and stddev 0.272295

hidden layer 7 had mean -0.000206 and stddev 0.254713

hidden layer 8 had mean 0.000148 and stddev 0.239710

hidden layer 9 had mean -0.000529 and stddev 0.227241

<!-- image -->

W = np.random.randn(in, out)/np.sqrt(in)

<!-- image -->

# Xavier initialization

input layer had mean 0.000422 and stddev 1.000922

hidden layer 0 had mean 0.397685 and stddev 0.583384

hidden layer 1 had mean 0.280004 and stddev 0.410386

hidden layer 2 had mean 0.183151 and stddev 0.280291

hidden layer 3 had mean 0.130450 and stddev 0.198090

hidden layer 4 had mean 0.092056 and stddev 0.138666

hidden layer 5 had mean 0.064787 and stddev 0.098349

hidden layer 6 had mean 0.047867 and stddev 0.069325

hidden layer 7 had mean 0.034165 and stddev 0.050724

hidden layer 8 had mean 0.024928 and stddev 0.035902

hidden layer 9 had mean 0.018272 and stddev 0.025658

W = np.random.randn(in, out)/np.sqrt(in)

<!-- image -->

<!-- image -->

<!-- image -->

# He initialization

- Problem with ReLu activation
- ReLU sets negative pre-activations to zero, which halves the effective variance of the signal
- To compensate, the initialization variance is doubled relative to Xavier
- Var(W) = 2 / fan\_in
- This keeps signal variance stable through deep networks with ReLu activations
- 

https://arxiv.org/abs/1502.01852 

# “Delving Deep into Rectifiers: Surpassing Human-Level Performance on ImageNet Classification” https://arxiv.org/pdf/1502.01852.pdf 

<!-- image -->

<!-- image -->

# He initialization

input layer had mean -0.001160 and stddev 1.000333

hidden layer 0 had mean 0.562934 and stddev 0.826029

hidden layer 1 had mean 0.577537 and stddev 0.828810

hidden layer 2 had mean 0.574250 and stddev 0.845599

hidden layer 3 had mean 0.602799 and stddev 0.851682

hidden layer 4 had mean 0.573220 and stddev 0.851573

hidden layer 5 had mean 0.548410 and stddev 0.811833

hidden layer 6 had mean 0.599740 and stddev 0.834997

hidden layer 7 had mean 0.586302 and stddev 0.870794

hidden layer 8 had mean 0.655965 and stddev 0.940311

hidden layer 9 had mean 0.637412 and stddev 0.874220

W = np.random.randn(in, out)/np.sqrt(in/2)

<!-- image -->

<!-- image -->

<!-- image -->

# Batch normalization	

- Simple idea: force the pre-activations of each layer to have zero mean and unit standard deviation before the activation function
- Compute mean and variance across each dimension
- Normalize

D

N

D1

D2

D3

# Batch normalization

<!-- image -->

algorithm source: https://arxiv.org/abs/1502.03167

Simple idea: just make the output of the linear units have 0 mean and unit standard deviation

Added them just before the activation function

# Batch normalization

Normalize:

Allow the network to modify the range

# Batch normalization

Normalize:

Allow the network to modify the range

Can learn the identity function

# Batch normalization at test time

- We cannot compute the mean and standard deviation on a batch of samples
- Instead we use a static mean and standard deviation were computed empirically during training 
- Compute the mean and standard deviation on the entire training set
- Estimate the mean and standard deviation using exponential weighted averages
- 
- 

# Batch normalization (advantages)

- Reduces the dependence on weights initialization
- Improves gradient flow through the network
- Slight regularization effect

# Bias and Variance

# Bias and Variance

- Bias: difference the average prediction of our model and the correct value which we are trying to predict
- Variance: the variability of model prediction/ the spread of the predictions
- 

# Bias and Variance

- High bias: under-fitting the data
- Model pays little attention to training data and over simplifies the problem
- High variance: over-fitting the data
- Model performs well on the training set, but not on test set

# Dealing with variance and bias problems

- High bias?
- 

# Dealing with variance and bias problems

- High bias? (train set performance)
- Try a more complex model/different NN architecture
- Train longer
- High variance?
- 
- 

# Dealing with variance and bias problems

- High bias? (train set performance)
- Try a more complex model/different NN architecture
- Train longer
- High variance? (validation set performance)
- Add more data
- Add more regularization techniques
- Try a different NN architecture
- 
- 

# Data pre-processing

# Importance of feature scaling

<!-- image -->

# Mean subtraction, scaling

X -= np.mean(X, axis=0)

X

X /= np.std(X, axis=0)

# Pre-processing for Images

- Zero center: subtract the mean across every individual feature in the data
- Mean image
- Mean across each channel

- 

<!-- image -->

# Regularization

Techniques to reduce overfitting and improve generalisation to unseen data

# Regularization	

- Remember the regularization techniques that we learned last time
- L1 regularization
- L2 regularization

# Regularization	

# Image augmentation

Image source:  https://mc.ai/easy-way-to-improve-image-classifier-performancepart-1%E2%80%8A-%E2%80%8Amixup-augmentation-with-codes-2/

# Cutout

- Randomly selects a rectangular patch and fills it with zeros (black). The label stays unchanged.
- Effect: Forces the model to use multiple regions of the image, not just the most discriminative part.
- Parameters: Patch size, number of patches, fill value (black or mean pixel).

<!-- image -->

Original (cat)

→

<!-- image -->

After CutOut

# MixUp 

- Blends two images pixel-by-pixel using coefficient λ. Labels are blended in the same proportion.
- x̃ = λ·xcat + (1−λ)·xdog     
- ỹ = λ·ycat + (1−λ)·ydog     
- Effect: Encourages linear behaviour between examples. Improves calibration.
- 
- 

<!-- image -->

Image A (cat)

× λ + 

<!-- image -->

Image B (dog)

× (1−λ)

=

<!-- image -->

Mixed (λ=0.6)

# Cutmix

- Cuts a region from image B (dog) and pastes it onto image A (cat). Labels are mixed by area ratio.
- Effect: Keeps spatial structure intact (unlike MixUp). Improves both localisation and regularisation.
- Parameters: patch size. Patch location is uniform random. 
- 

<!-- image -->

Image A (cat)

+

<!-- image -->

Image B (dog)

=

<!-- image -->

Result (dog patch on cat)

Mixed label:

ỹ = λ·ycat + (1−λ)·ydog

λ = non-cut area ratio

# Cutout. Mixup. CutMix

|                        | Cutout                | Mixup          | CutMix                  |
|------------------------|-----------------------|----------------|-------------------------|
| Input image alteration | Erase a region        | Blend pixels   | Swap regions            |
| Modifies labels?       | No                    | Yes            | Yes                     |
| Spatial structure?     | Partial               | Lost           | Preserved               |
| Label type             | Hard (original label) | Soft (blended) | Soft (by replaced area) |

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

https://arxiv.org/pdf/1905.04899 

# Dropout

# Dropout (2014)

Randomly drop units (and their connections) from the neural network during training

<!-- image -->

SRIVASTAVA, Nitish, et al. Dropout: a simple way to prevent neural networks from overfitting. The journal of machine learning research, 2014, 15.1: 1929-1958.

https://jmlr.org/papers/volume15/srivastava14a/srivastava14a.pdf 

# Dropout

<!-- image -->

<!-- image -->

keep\_prop – probability of keeping a neuron; larger value, less dropout

# Dropout intuition

- At each iteration you actually modify your model:
- train a different model that uses only a subset of your neurons
- neurons thus become less sensitive to the activation of one other specific neuron (others neuron might be shut down at any time)

# Dropout intuition

- Train a large ensemble of models (with shared parameters)
- Forces the model to have a redundant representation

<!-- image -->

# Dropout at test time

<!-- image -->

# Dropout at test time

- The neurons are always turned on
- Scale the activation for each neuron
- output at test time = expected output at training time

# Dropout at test time

keep\_prop – probability of keeping a neuron; larger value, less dropout

<!-- image -->

# Dropout in practice

<!-- image -->

A hidden unit cannot rely on other specific units to correct its mistakes. It must perform well in a wide variety of different contexts provided by the other hidden units.

# Optional additional reading

- https://ruder.io/optimizing-gradient-descent/index.html#otherrecentoptimizers 
- https://www.youtube.com/watch?v=k8fTYJPd3\_I 
- https://www.youtube.com/watch?v=\_e-LFe\_igno 
- https://www.coursera.org/lecture/deep-neural-network/the-problem-of-local-optima-RFANA 