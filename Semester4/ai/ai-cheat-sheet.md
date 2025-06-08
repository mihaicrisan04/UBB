
### 1. Foundations of Artificial Intelligence & Problem Solving

Artificial Intelligence (AI) is the field dedicated to creating computational systems that can perform tasks requiring human-like intelligence. A major subfield, **Machine Learning (ML)**, focuses on algorithms that allow computers to learn from data without being explicitly programmed.

A learning problem is formally defined by three components:
*   **Task (T):** The specific job the system needs to perform (e.g., playing checkers, classifying emails).
*   **Performance Measure (P):** A metric to evaluate how well the system performs the task (e.g., percentage of games won, accuracy of email classification).
*   **Experience (E):** The data or interactions the system uses to learn and improve (e.g., playing games against itself, a database of labeled emails).

The core of many AI problems is **search**. This involves defining a problem space and navigating it to find a solution.

*   **Problem Definition Components:**
    *   **Search Space:** The set of all possible states the problem can be in.
    *   **Initial State:** The starting configuration.
    *   **Final (Goal) State(s):** The desired outcome(s).
    *   **Operators (Successor Functions):** Rules that define valid transitions between states.
    *   **Cost Function:** Assigns a cost to moving between states or along a path.
*   **Evaluation of Search Strategies:**
    *   **Completeness:** Guarantees finding a solution if one exists.
    *   **Optimality:** Guarantees finding the best (lowest cost) solution.
    *   **Time Complexity:** The time required to find a solution, often expressed in terms of \(b\) (branching factor) and \(d\) (depth of solution).
    *   **Space Complexity:** The amount of memory required during the search.

### 2. Uninformed Search Strategies

These "blind" search algorithms explore the search space systematically without any knowledge of the distance to the goal.

*   **Breadth-First Search (BFS):** Explores the search tree level by level. It uses a First-In, First-Out (FIFO) queue to manage nodes. It is complete and optimal if all step costs are equal, but its time and space complexity of \(O(b^d)\) makes it impractical for large problems.
*   **Uniform-Cost Search (UCS):** A modification of BFS that expands the node with the lowest path cost from the start, rather than the shallowest node. It is complete and optimal but can be inefficient.
*   **Depth-First Search (DFS):** Explores as deeply as possible along one path before backtracking. It uses a Last-In, First-Out (LIFO) stack. It is very memory efficient (\(O(b \cdot d)\)) but is neither complete (can get trapped in infinite paths) nor optimal.
*   **Depth-Limited Search (DLS):** A variation of DFS that imposes a maximum depth limit to prevent infinite paths, thus solving the completeness issue. However, it is only optimal if the solution is found at the depth limit.
*   **Iterative Deepening Depth-First Search (IDDS):** Combines the strengths of BFS and DFS. It performs a series of DLS searches with progressively increasing depth limits. It is complete, optimal, and has the low memory requirements of DFS.
*   **Bi-Directional Search (BDS):** Runs two searches simultaneously: one forward from the initial state and one backward from the goal state. The search stops when the two frontiers meet. This can significantly reduce the search time, with a complexity of \(O(b^{d/2})\).

### 3. Machine Learning Paradigms

ML algorithms are categorized by the type of data and feedback they use for learning.

*   **Supervised Learning:** The algorithm learns from a dataset where each example is labeled with the correct output. The goal is to learn a mapping function from inputs to outputs.
    *   **Example:** Given a dataset of emails labeled as "spam" or "not spam," learn to classify new, unlabeled emails.
*   **Unsupervised Learning:** The algorithm learns from an unlabeled dataset, seeking to discover hidden structures or patterns.
    *   **Example:** Grouping customers into distinct segments based on their purchasing behavior, without any pre-existing labels for these segments.
*   **Reinforcement Learning:** An "agent" learns by interacting with an environment. It performs actions and receives rewards or penalties, learning a policy that maximizes its cumulative reward over time.
    *   **Example:** Training an AI to play a game like chess by rewarding it for winning and penalizing it for losing.

### 4. Supervised Learning Algorithms

#### 4.1. Core Concepts and Evaluation

*   **Data Handling:** A dataset is typically split into a **training set** (e.g., 80%) to build the model and a **test set** (e.g., 20%) to evaluate its performance on unseen data. This helps diagnose **overfitting**, where a model learns the training data too well and fails to generalize.
*   **Key Evaluation Metrics:**
    *   **Confusion Matrix:** A table summarizing classification performance:
        *   **True Positives (TP):** Correctly predicted positive instances.
        *   **False Positives (FP):** Negative instances incorrectly predicted as positive.
        *   **True Negatives (TN):** Correctly predicted negative instances.
        *   **False Negatives (FN):** Positive instances incorrectly predicted as negative.
    *   **Accuracy:** \( \frac{TP+TN}{Total} \). The proportion of correct predictions.
    *   **Precision:** \( \frac{TP}{TP+FP} \). Of all positive predictions, how many were correct?
    *   **Recall:** \( \frac{TP}{TP+FN} \). Of all actual positive instances, how many were identified?
    *   **F1 Score:** \( 2 \cdot \frac{Precision \cdot Recall}{Precision + Recall} \). The harmonic mean of precision and recall, useful for imbalanced classes.
    *   **Mean Squared Error (MSE):** For regression, it measures the average squared difference between predicted (\(\hat{y}_i\)) and actual (\(y_i\)) values: \( MSE = \frac{1}{N} \sum_{i=1}^{N} (y_i - \hat{y}_i)^2 \).

#### 4.2. Decision Trees (DT)

A DT is a flowchart-like structure used for classification and regression.

*   **Structure:** Internal nodes represent tests on attributes, branches represent outcomes, and leaf nodes represent class labels or continuous values.
*   **Construction (ID3/C4.5):** The tree is built top-down. At each node, the algorithm selects the attribute that provides the most **Information Gain**—the one that best splits the data into purer, more homogeneous subsets.
    *   **Entropy:** A measure of impurity in a set \(S\). For a set with \(k\) classes, it is:
        $$ E(S) = \sum_{i=1}^{k} -p_i \log_2 p_i $$
        where \(p_i\) is the proportion of examples belonging to class \(i\). Entropy is 0 for a pure set and 1 for a set with a 50/50 split (in a two-class problem).
    *   **Information Gain:** The reduction in entropy achieved by splitting on an attribute \(a\):
        $$ Gain(S, a) = E(S) - \sum_{v \in values(a)} \frac{|S_v|}{|S|} E(S_v) $$
*   **Pruning:** To combat overfitting, branches that provide little predictive power are removed.

#### 4.3. Artificial Neural Networks (ANNs) and Deep Learning

ANNs are computing systems inspired by the biological brain, composed of interconnected nodes (neurons) in layers.

*   **The Perceptron:** The simplest neuron model. It computes a weighted sum of its inputs and applies a step function (activation) to produce an output. It can only solve linearly separable problems.
*   **Feed-Forward ANNs:** Information flows from an input layer, through hidden layers, to an output layer. The connections between neurons have associated **weights**, which are the parameters the network learns during training.
*   **Activation Functions:** These introduce non-linearity, allowing the network to learn complex patterns.
    *   **Sigmoid & Tanh:** Historically popular but suffer from the "vanishing gradient" problem, where gradients become extremely small, halting learning in deep networks.
    *   **ReLU (Rectified Linear Unit):** \(f(x) = \max(0, x)\). The modern default for hidden layers. It is computationally simple and avoids the vanishing gradient problem for positive inputs.
*   **Training with Backpropagation:** ANNs are trained using **gradient descent**.
    1.  **Forward Pass:** An input is passed through the network to generate a prediction.
    2.  **Loss Calculation:** The prediction is compared to the true label using a **loss function** (e.g., MSE for regression, Cross-Entropy for classification).
    3.  **Backward Pass (Backpropagation):** The error is propagated backward through the network, calculating the gradient of the loss with respect to each weight.
    4.  **Weight Update:** Each weight is adjusted slightly in the opposite direction of its gradient to reduce the error: \( w_{new} = w_{old} - \eta \frac{\partial E}{\partial w_{old}} \), where \(\eta\) is the learning rate.
*   **Deep Learning & CNNs:** Deep learning uses ANNs with many hidden layers. This depth allows the model to learn a hierarchy of features. **Convolutional Neural Networks (CNNs)** are a special type of deep network for image analysis.
    *   **Convolutional Layer:** Applies filters (kernels) to an image to detect features like edges, textures, and shapes, creating "feature maps."
    *   **Pooling Layer:** Reduces the size of feature maps (e.g., Max Pooling takes the maximum value in a window), making the model more efficient and robust to variations in the feature's position.

#### 4.4. Support Vector Machines (SVM)

SVMs are powerful classifiers that find the optimal hyperplane to separate two classes.

*   **Maximal Margin Hyperplane:** The best hyperplane is the one that has the largest possible distance (margin) to the nearest data points from either class. These nearest points are called **support vectors**. Maximizing the margin improves the model's ability to generalize to new data.
*   **The Kernel Trick:** For data that is not linearly separable, SVMs use a **kernel function** (e.g., Polynomial, RBF) to project the data into a higher-dimensional space where a separating hyperplane can be found. This is done efficiently without explicitly calculating the new coordinates.

### 5. Unsupervised Learning Algorithms

#### 5.1. K-Means Clustering

An algorithm to partition \(N\) data points into \(k\) distinct, non-overlapping clusters.

*   **Algorithm Steps:**
    1.  **Initialize:** Randomly select \(k\) data points to be the initial cluster centroids.
    2.  **Assign:** Assign each data point to the cluster with the nearest centroid (e.g., using Euclidean distance).
    3.  **Update:** Recalculate each centroid as the mean of all points assigned to its cluster.
    4.  **Repeat:** Iterate the assignment and update steps until the cluster assignments stabilize.
*   **Characteristics:** It is simple and fast but requires the number of clusters, \(k\), to be specified in advance and can be sensitive to the initial random placement of centroids.

### 6. Nature-Inspired Optimization

These algorithms use metaphors from nature to solve complex optimization and search problems.

#### 6.1. Evolutionary Algorithms (EAs)

EAs are based on the principles of biological evolution.

*   **Core Process:** A **population** of candidate solutions (**individuals**) is evolved over generations.
    *   **Representation:** Each solution is encoded as a **chromosome** (e.g., a bit-string).
    *   **Fitness Function:** Evaluates how good each solution is.
    *   **Selection:** Fitter individuals are more likely to be selected for reproduction.
    *   **Crossover:** Creates new offspring by combining the chromosomes of two parents.
    *   **Mutation:** Introduces small, random changes into a chromosome to ensure genetic diversity.

#### 6.2. Particle Swarm Optimization (PSO)

Inspired by the flocking of birds, PSO uses a swarm of **particles** to explore the search space.

*   **Movement:** Each particle adjusts its position and velocity based on three factors:
    1.  Its own momentum (inertia).
    2.  Its own best position found so far (\(p_{Best}\)).
    3.  The best position found by any particle in the swarm so far (\(g_{Best}\)).

#### 6.3. Ant Colony Optimization (ACO)

Modeled on the foraging behavior of ants, ACO is excellent for finding optimal paths in graphs.

*   **Pheromone Trails:** Artificial ants navigate a graph, depositing "pheromones" on the paths they take. Shorter paths are traversed more quickly, accumulating pheromones faster.
*   **Probabilistic Choice:** Ants choose their next step based on a combination of the pheromone trail strength and a heuristic (e.g., the distance to the next node). Over time, the pheromone on the optimal path becomes strongest, guiding the entire colony to the best solution.

### 7. Handling Uncertainty with Fuzzy Logic

Fuzzy logic provides a framework for reasoning with imprecise or vague concepts, moving beyond the binary true/false of classical logic.

*   **Fuzzy Sets:** An element's membership in a set is a matter of degree, represented by a value in \([0, 1]\).
    *   **Example:** A room temperature of 23°C might have a membership of 0.7 in the fuzzy set "warm" and 0.3 in the fuzzy set "cool."
*   **Fuzzy Inference System:**
    1.  **Fuzzification:** Converts a crisp input (e.g., temperature = 23°C) into degrees of membership in fuzzy sets.
    2.  **Rule Evaluation:** Applies IF-THEN rules using fuzzy operators (e.g., `min` for AND, `max` for OR). For example: `IF temperature is warm AND humidity is high THEN fan_speed is fast`.
    3.  **Aggregation:** Combines the fuzzy outputs of all rules into a single fuzzy set.
    4.  **Defuzzification:** Converts the aggregated fuzzy output back into a single, crisp number (e.g., fan speed = 85%), often by calculating the center of gravity (centroid) of the fuzzy set.