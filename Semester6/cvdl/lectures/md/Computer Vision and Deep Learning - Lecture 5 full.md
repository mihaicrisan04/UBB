# Computer Vision and Deep Learning

Lecture 5

# Today’s agenda

- Recap: conv nets
- Attention modules in CNNs
- SENet recap
- CBAM
- Efficient neural networks
- Mobilenet
- Shuffle net
- Visualization and interpretability

# Recap - Convolutions

https://www.youtube.com/watch?v=vVaRhZXovbw 

# Recap

- Convolutional layer
- How does it work
- Hyperparameters
- Number of learnable parameters
- 
- Pooling layers
- How does it work
- Hyperparameters
- Number of learnable parameters

- Global Average Pooling

# Recap

- CNN architectures
- Alexnet
- VGG
- Inception
- ResNet

# Attention 

- In human vision, we don't process every part of a scene equally: we selectively focus on what matters.
- Attention in neural networks formalises this: learn to weight features by their relevance, rather than treating all of them uniformly.

<!-- image -->

<!-- image -->

# Attention

- Attention in neural networks: learn to weight features by their relevance, rather than treating all of them uniformly.
- Given a feature map F ∈ ℝC×H×W, an attention mechanism produces a mask M of the same or compatible shape, and outputs F' = M ⊗ F; ⊗ is element-wise multiplication.

# Attention in CNNs

- Convolutional filters have:
- Locally receptive: a 3×3 kernel sees only a 3×3 patch in the input
- Weight-shared: the same filter is applied everywhere, regardless of content
- Statically weighted: channel importance is fixed after training and cannot adapt to the content of a specific input at inference time.
- Standard CNN cannot dynamically suppress irrelevant features or amplify informative ones at inference time.
- 

# Attention in CNNs

| Axis    | Question                            | Architecture                |
|---------|-------------------------------------|-----------------------------|
| Channel | Which features are more important?  | Squeeze and Excitation nets |
| Spatial | Where in the image matters?         | CBAM                        |
| Both    | Which features? Where in the image? | CBAM                        |

# Attention in CNNs

- Let F ∈ ℝ(C×H×W) be the input feature map. An attention module learns a function:

                          A : ℝ(C×H×W) → ℝ(C×H×W), such that F' = A(F) ⊗ F

This can be decomposed as:

- Channel attention: M\_c ∈ ℝ (C×1×1): one scalar weight per channel
- Spatial attention: M\_s ∈ ℝ(1×H×W): one scalar weight per spatial location
- Combined: apply sequentially or in parallel
- These weights are input-dependent: they are re-computed for every forward pass.
- They are lightweight additions inserted between existing CNN block and add negligible parameters relative to the backbone
- 

# SENet – Channel attention

<!-- image -->

Squeeze-and-Excitation Networks, 2018: https://arxiv.org/pdf/1709.01507.pdf  

ImageNet 2017 winner

2.3% top-5 error

<!-- image -->

Compute weight for each feature map:

- Global average pooling
- 2FC layers

# SENet

Squeeze (Global Information Embedding) squeeze global spatial information into a channel descriptor

Excitation (Adaptive Recalibration) 

- capture channel-wise dependencies
- dynamics conditioned on the input
- 
- 
- 
- 
- 
- 

<!-- image -->

<!-- image -->

<!-- image -->

# CBAM – Convolutional Block Attention Module (Channel and Spatial attention)

<!-- image -->

https://openaccess.thecvf.com/content\_ECCV\_2018/papers/Sanghyun\_Woo\_Convolutional\_Block\_Attention\_ECCV\_2018\_paper.pdf 

Convolutional Block Attention Module (CBAM):

- a simple yet effective attention module for feed-forward convolutional neural networks

<!-- image -->

# Channel Attention

- GAP(Global Average Pooling)
- Aggregate spatial information
- GMP
- Preserve richer context information
- Multilayer perceptron (MLP)
- Sigmoid activation 
- Gives the weights for each channel

<!-- image -->

https://openaccess.thecvf.com/content\_ECCV\_2018/papers/Sanghyun\_Woo\_Convolutional\_Block\_Attention\_ECCV\_2018\_paper.pdf 

# Spatial Attention

- Two pooling operations across the channel axis
- 7x7  Conv
- Sigmoid activation
- Can be applied element-wise to all the positions in the input feature map

<!-- image -->

<!-- image -->

https://openaccess.thecvf.com/content\_ECCV\_2018/papers/Sanghyun\_Woo\_Convolutional\_Block\_Attention\_ECCV\_2018\_paper.pdf 

<!-- image -->

https://pytorch.org/docs/stable/generated/torch.nn.AdaptiveMaxPool2d.html 

<!-- image -->

# Efficient neural networks

MobileNet

ShuffleNet

EfficientNet

# MobileNet

- Efficient models for mobile and embedded vision applications

<!-- image -->

https://arxiv.org/pdf/1704.04861.pdf 

# Depth-wise separable convolutions

<!-- image -->

<!-- image -->

M: Number of input channels, 

N: Number of output channels, 

DK: Kernel size

DF: Feature map size

vs. 

<!-- image -->

Operation cost:

Depthwise convolution

Classical convolution

# MobileNet

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

M: Number of input channels, 

N: Number of output channels, 

DK: Kernel size

DF: Feature map size

# Depth-wise separable convolutions

# Depth-wise separable convolutions

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

- α : width multiplier thin a network uniformly at each layer
- # input channels M becomes αM 
- # output channels N becomes αN
- ρ resolution multiplier: in practice, it is set by setting the input resolution

<!-- image -->

Shallow Mobilenet: 5 layers of separable filters with feature size 14 × 14 × 512  are removed

# ShuffleNet

- Classic convolutions
- Number of operations: output\_channels × input\_channels × kernel\_size² × spatial\_size
- e.g. 3x3 filter with 256 input channels and  256 output channels:
- 256 × 256 × 3 × 3 × spatial\_size
- 
- Depthwise separable convolutions
- Depthwise conv: apply one filter per channel, only looking at spatial neighbours. No cross-channel mixing at all.
- Pointwise conv (1×1):  mix across channels using a 1×1 convolution with no spatial extent.
- The pointwise (1×1) convolution is still expensive: every output channel still connects to every input channel. This accounts for the vast majority of total compute.
- 
- 

https://arxiv.org/pdf/1707.01083 

# ShuffleNet

- Classic convolutions
- Depthwise separable convolutions
- Depthwise conv: apply one filter per channel, only looking at spatial neighbours. No cross-channel mixing at all.
- Pointwise conv (1×1):  mix across channels using a 1×1 convolution with no spatial extent.
- The pointwise (1×1) convolution is still expensive: every output channel still connects to every input channel. This accounts for the vast majority of total compute.
- 
- 

https://arxiv.org/pdf/1707.01083 

# ShuffleNet

- Group convolutions
- instead of connecting every output channel to every input channel, split channels into g independent groups and only connect within each group
- E.g. g = 4, 256 input channels, 256 output channels
- Input: 4 groups of 64
- Output 4 groups of 64
- Cost reduction: 4 × (64 × 64) = 256 × 64 vs 256 × 256; this is exactly 4× cheaper than before
- 
- 
- 
- 

https://arxiv.org/pdf/1707.01083 

# ShuffleNet

https://arxiv.org/pdf/1707.01083 

<!-- image -->

Problem: if group 1 never talks to groups 2or 3, information becomes siloed. 

Each output group only sees its corresponding input group

Representation quality degrades.

# ShuffleNet 

- Channel shuffle solves the silo problem with zero learnable cost.
- After each group convolution, reorder the channels so that the next layer's groups each receive channels from all of the previous groups.

Given: feature map of shape (g×n, H, W) 

1. Reshape: (g×n, H, W) → (g, n, H, W) 
2. Transpose: (g, n, H, W) → (n, g, H, W)
3. Flatten: (n, g, H, W) → (g×n, H, W) 

Feed result into next group convolution layer.

# ShuffleNet unit

<!-- image -->

(a) Starting point: ResNet redisual unit

(b) ShuffleNet unit: replace 1 × 1 layer with pointwise group convolution + channel shuffle

(c) ShuffleNet unit with stride:  (i) add a 3 × 3 average pooling on the shortcut path;  (ii) replace the element-wise addition with channel concatenation

# ShuffleNet unit

<!-- image -->

ShuffleNet vs ResNet:

Given the input size c × h × w and the bottleneck channels m: 

- ResNet unit: hw(2cm + 9m2 ) FLOPs 
- ShuffleNet unit: hw(2cm/g + 9m) FLOPs, g – number of groups

# ShuffleNet

https://arxiv.org/pdf/1707.01083 

<!-- image -->

Adapt the output channels to ensure computation cost roughly unchanged

# ShuffleNet

https://arxiv.org/pdf/1707.01083 

<!-- image -->

<!-- image -->

<!-- image -->

sx – width mutiplier

# Adversarial examples

<!-- image -->

<!-- image -->

# Adversarial examples

An adversarial example is an instance with small, intentional feature perturbations that cause a machine learning model (not only CNNs) to make a false prediction.

<!-- image -->

# Gradient sign-method

- Use the gradient to compute the adversarial example
- A small perturbation in the direction of the gradient accumulates across thousands of dimensions
- Even tiny ε per pixel → large effect on the output when summed over all pixels
- 
- 
- The sign of the gradient is positive (+1) if an increase in pixel intensity increases the loss (the error the model makes) and negative (-1) if a decrease in pixel intensity increases the loss

<!-- image -->

# Gradient sign method

<!-- image -->

# Make everything an ostrich

- Start with a clean image x and target class l ("ostrich"); r = 0 (no perturbation)
- Freeze the model weights
- Compute the gradient of the loss w.r.t. the input pixels
- 
- Update the perturbation r to maximize confidence in the target class
- Constrain |r| to be small (imperceptible)

Szegedy, Christian, et al. "Intriguing properties of neural networks." arXiv preprint arXiv:1312.6199 (2013).

<!-- image -->

# Classifying noise

- No real image needed: start from scratch and optimize pixels toward a target class
- Models learn non-human features

Nguyen, Anh, Jason Yosinski, and Jeff Clune. "Deep neural networks are easily fooled: High confidence predictions for unrecognizable images." Proceedings of the IEEE conference on computer vision and pattern recognition. 2015.

# 1-pixel attacks

Constraint: when designing the adversarial example only one pixel may change

<!-- image -->

<!-- image -->

One Pixel Attack for Fooling Deep Neural Networks, Su et al. , 2019

# 1-pixel attacks

- Uses differential evolution an algorithm loosely inspired by the biological evolution of species. 
- A population of individuals (candidate solutions) recombines generation by generation until a solution is found. 
- Each candidate solution encodes a pixel modification and is represented by a vector of five elements: x, y, r, g, b values

https://en.wikipedia.org/wiki/Differential\_evolution

# Adversarial patch Make everything a toaster

<!-- image -->

https://doi.org/10.48550/arXiv.1712.09665 

# Physical adversarial perturbations

- Perturbations cause a neural network to misclassify stop signs as speed limit 45 signs, and right turn signs as stop signs

<!-- image -->

# Physical adversarial perturbations

- Camouflage graffiti and art stickers cause a neural network to misclassify stop signs as speed limit 45 signs or yield signs.

https://spectrum.ieee.org/cars-that-think/transportation/sensors/slight-street-sign-modifications-can-fool-machine-learning-algorithms 

<!-- image -->

#  Robust adversarial examples

<!-- image -->

https://arxiv.org/pdf/1707.07397 

# Understanding what networks learn

Much learning does not teach understanding. - Heraclitus

<!-- image -->

# Interpretability vs. Explainability

- The terms are often used interchangeably, but they refer to different concepts

|            | Interpretable                                      | Explainable                                                                    |
|------------|----------------------------------------------------|--------------------------------------------------------------------------------|
| Definition | The model's mechanics are understandable by design | A separate method produces an approximation of why the model behaved as it did |
| When?      | Before / during training                           | After training (post hoc)                                                      |
| Examples   | Linear regression, decision trees                  | Grad-CAM, LIME, SHAP                                                           |
|            | Lower capacity, higher transparency                | Higher capacity, approximate explanations                                      |

# Interpretability vs. Explainability

- CNNs achieve state-of-the-art performance on vision tasks,  but performance is not always sufficient
- A radiologist needs to know why a model flagged a tumour, not just that it did
- A researcher needs to verify the model is using the right features, not just  correlations (e.g. classifying tanks by the cloudy sky)
- A CNN is not interpretable: you cannot read off its reasoning from its weights
- A CNN result can be explained using auxiliary techniques
- 

# Saliency maps via image occlusions

<!-- image -->

<!-- image -->

Which are the important parts of an image?

Slide an occluding patch over the input image and display (as a heat map) the probability of the correct class

# Saliency maps via image occlusions

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

P(cat) = 0.4

P(cat) = 0.96

# Filter visualization – 1st layer

First layer operates directly on image pixels; visualize the filters used to extract the image features

Visualize this filter bank

https://machinelearningmastery.com/how-to-visualize-filters-and-feature-maps-in-convolutional-neural-networks/ 

# Filter visualization – 1st layer

First layer operates directly on image pixels; visualize the filters used to extract the image features

<!-- image -->

# Filter visualization – deeper layers 

- Not that easy to interpret

<!-- image -->

<!-- image -->

https://cs.stanford.edu/people/karpathy/convnetjs/demo/mnist.html 

# Last layer visualization

- The last layer (before the classification layer) contains the most condensed representation of the image

<!-- image -->

# Last layer visualization Image embedding

<!-- image -->

Five ILSVRC-2010 test images in the first column. The remaining columns show the six training images that produce feature vectors in the last hidden layer with the smallest Euclidean distance from the feature vector for the test image

# Visualize the patch that maximally activates a neuron

<!-- image -->

https://arxiv.org/pdf/1311.2524.pdf 

- Single out a particular unit (feature) in the network and use it as if it were an object detector in its own right
- compute the unit’s activations on a large set of held-out region, sort the proposals from highest to lowest activation, perform non-maximum suppression, and then display the top-scoring region

# Saliency maps

https://arxiv.org/pdf/1312.6034.pdf 

<!-- image -->

# Saliency maps

https://arxiv.org/pdf/1312.6034.pdf 

# Saliency maps – visualize the data gradient Important image pixels

<!-- image -->

https://arxiv.org/pdf/1312.6034.pdf 

# Class model visualization Optimize the image

- Keep the parameters of the network fixed and optimize the input image for some class score

<!-- image -->

Zero image

<!-- image -->

# Class model visualization Optimize the image

Zero image

<!-- image -->

# Class model visualization Optimize the image

https://arxiv.org/pdf/1312.6034.pdf 

<!-- image -->

<!-- image -->

# Deep Dream

# Deep dream

- “Dream” at a given layer of the network (after the ReLu units)
- Forward pass to the give image
- Set the gradients to be equal to the activation
- Backprop to the image
- 
- Modify the image such that it amplifies the activations at any given layer 
- Some sort of CNN pareidolia

https://www.youtube.com/watch?v=SCE-QeDfXtA&amp;t=57s 

https://photos.google.com/share/AF1QipPX0SCl7OzWilt9LnuQliattX4OUCj\_8EP65\_cTVnBmS1jnYgsGQAieQUc1VQWdgQ?pli=1&amp;key=aVBxWjhwSzg2RjJWLWRuVFBBZEN1d205bUdEMnhB 

# Deep dream

Whatever was activates gets boosted

<!-- image -->

# Deep dream

<!-- image -->

# Deep visualization toolbox #deepvis

https://www.youtube.com/watch?v=AgkfIQ4IGaM 

# Synthesising What a Neuron Wants to See

- Show the top-n images from the training set that cause the highest activation for a chosen neuron
- You only see what the neuron responds to within the constraints of your dataset, not what it truly prefers in general.
- 

# Synthetising what a neuron wants to see

# Synthetising what a neuron wants to see

# Class activation map (CAM)

- Only works for architectures that use Global Average Pooling (GAP) before the final classification layer. 
- Required architecture:

Conv Layers → Last Conv Feature Maps → Global Average Pooling → FC Layer → Softmax

- 

https://arxiv.org/pdf/1512.04150 

<!-- image -->

# CAM

# CAM

<!-- image -->

# CAM

- The last convolutional layer produces a grid of feature maps: each one a detector for some visual concept.
- The final classifier learns a set of weights:  which feature maps are important or not for predicting a class
- If we know which feature maps matter for a class, and we know where those feature maps activate spatially, we can combine them to find where in the image the evidence for that class is located.
- Limitation: only works for network with GAP

GAP layer preserves a direct link between the classifier weights and the spatial feature maps. 

- 

# CAM

<!-- image -->

# GradCAM

- Generalize CAM to any CNN architecture without modifying or retraining the model.
- Use the gradients of the class score with respect to the feature maps of any target convolutional layer as a proxy for the importance weights  
- replaces the need for GAP weights
- The last convolutional layers have the best compromise between high-level semantics and detailed spatial information

https://arxiv.org/abs/1610.02391

# GradCAM

<!-- image -->

# GradCAM

<!-- image -->

# GradCAM

<!-- image -->

# CAM vs GradCAM

- GradCAM averages gradients over all spatial locations to get a single importance number per feature map (just like CAM's weights, but derived from gradients instead)
- The gradient of the class score with respect to a feature map measures how sensitive the prediction is to that feature map.
- features whose activation strongly influences the class score are important, and where they activate spatially tells us where the model is looking.

# GradCAM

<!-- image -->

<!-- image -->

Adversarial examples

Nurse vs. doctor: biased and unbiased model

# “Why Should I Trust You?” Explaining the Predictions of Any Classifier

<!-- image -->

https://arxiv.org/pdf/1602.04938

# “Why Should I Trust You?” Explaining the Predictions of Any Classifier

<!-- image -->

# Locally Interpretable Model-agnostic Explanations (LIME)

- Understand the model’s behaviour right around one decision
- Idea: Alter the input, watch what changes, and fit a simple explanation to what you observe.
- Alter - randomly hiding parts of the image (turning super-pixels gray)
- If hiding a region dramatically changes the prediction, then that region was important
- If hiding a region changes nothing, then the model wasn't paying attention to it

# LIME notation

<!-- image -->

# LIME

# LIME – choosing G

<!-- image -->

<!-- image -->

# LIME

# LIME

<!-- image -->

<!-- image -->

# LIME – "Husky vs. Wolf" Case Study

- Classifier trained on 20 images: all wolves had snow backgrounds, all huskies did not
- Features: first max-pooling layer of Inception
- Classifier predicts Wolf whenever there is snow/light background

<!-- image -->

<!-- image -->

# LIME limitations

- Certain interpretable representations will not be powerful enough to explain certain behaviors
- a model that predicts sepia-toned images to be retro cannot be explained by presence of absence of super pixels
- If f is highly non-linear even locally, a linear g may not be a faithful explanation
- Super-pixel quality affects explanation quality

# xAI resources

- https://medium.com/@gabriele.ciravegna/c-xai-concept-based-explainable-ai-51dece0472f1 
- https://www.youtube.com/watch?v=Yg3q5x7yDeM 
- https://christophm.github.io/interpretable-ml-book 
- https://www.youtube.com/watch?v=UzKdulNKoDY&amp;list=PLqDyyww9y-1QA4-o4tTAF\_iD5cKCC1qEA 