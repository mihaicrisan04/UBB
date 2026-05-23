# Computer vision and Deep Learning

Lecture 4: Convolutional neural networks

BORZA DIANA LAURA

# Today’s agenda

- Convolutional neural networks
- Evolution of CNNs
- 1st reading group: Efficient nets

# Visual cortex

Nobel Prize for Physiology or Medicine in 1981:

David Hubel and Torsten Wiesel 

<!-- image -->

Image source: https://www.researchgate.net/publication/335707980\_Understanding\_Neural\_Networks\_via\_Feature\_Visualization\_A\_Survey 

Simple cells: 

	orientation, position

Complex cells: 

	orientation, motion, direction

“Hypercomplex” cells:

	orientation, motion, direction, length

Hubel and Wiesel, 1959

https://www.youtube.com/watch?v=IOHayh06LJ4&amp;ab\_channel=PaulLester 

# Understanding the visual cortex

- Nearby cells in the cortex represented and processed nearby regions in the visual field 
- Hubel and Wiesel hypothesized that the visual cortex can be described by a hierarchical organization of simple cells that fed into complex cells which have more complicated activations and can form higher level representations

<!-- image -->

https://peterkim95.github.io/jekyll/update/2017/08/29/introspectiveness-of-nn/ 

# Neocognitron

https://www.youtube.com/watch?v=W8SKi6J\_YYU&amp;t=4s 

<!-- image -->

# Neocognitron, 1979 Ancestor of CNNs

- Hierarchical Feature Extraction: 
- S-layers (simple cells, feature detectors) and C-layers (complex cells, spatial poolers) build increasingly abstract representations from local to global.
- Shift &amp; Deformation Tolerance:
-  C-cells pool over a neighborhood of S-cells, granting tolerance to small translations and distortions
- Direct Ancestor of CNNs: 
- LeCun's LeNet (1989) adopted the S/C duality as conv + pooling layers

# Building blocks of CNNs

- Recap

# Image convolutions

|   10 |   20 |   30 |   15 |   12 |
|------|------|------|------|------|
|   11 |  200 |   34 |   23 |   45 |
|   32 |   35 |  255 |  255 |   10 |
|   10 |   33 |    6 |    7 |   59 |
|   13 |   43 |   13 |   45 |    3 |

|   -1 |   -1 |   -1 |
|------|------|------|
|    0 |    0 |    0 |
|    1 |    1 |    1 |

# Image convolutions

|   10 |   20 |   30 |   15 |   12 |
|------|------|------|------|------|
|   11 |  200 |   34 |   23 |   45 |
|   32 |   35 |  255 |  255 |   10 |
|   10 |   33 |    6 |    7 |   59 |
|   13 |   43 |   13 |   45 |    3 |

|   -1 |   -1 |   -1 |
|------|------|------|
|    0 |    0 |    0 |
|    1 |    1 |    1 |

| 262   |    |    |
|-------|----|----|
|       |    |    |
|       |    |    |

-10 – 20 – 30 + 32 + 35 + 255 = 262

# Image convolutions

|   10 |   20 |   30 |   15 |   12 |
|------|------|------|------|------|
|   11 |  200 |   34 |   23 |   45 |
|   32 |   35 |  255 |  255 |   10 |
|   10 |   33 |    6 |    7 |   59 |
|   13 |   43 |   13 |   45 |    3 |

|   -1 |   -1 |   -1 |
|------|------|------|
|    0 |    0 |    0 |
|    1 |    1 |    1 |

| 262   | 480   |    |
|-------|-------|----|
|       |       |    |
|       |       |    |

-20-30-15+35+255+255 = 480 

# Convolutional filters What do you think is the effect of the following filters?

# Convolutional filters What do you think is the effect of the following filters?

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# Convolutional filters What do you think is the effect of the following filters?

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

Gabor filters

<!-- image -->

Kernels learned from AlexNet first convolutional layers

# Convolutional layers

- Preserve the spatial information
- Convolve the filter over the entire input volume
- The filter has the same depth as the input 

<!-- image -->

Input volume

32x32x3

filter

5x5x3

Activation map

28x28x1

# Convolutional layers

- Preserve the spatial information
- Convolve the filter over the entire input volume
- The filter has the same depth as the input 

Input volume

32x32x3

filter

5x5x3

Activation map

28x28x1

At each position in the input volume:

Multiply (element-wise, across all channels) the filter and a small patch (5x5x3) in this input volume and add a bias term

# Convolutional layers

- Preserve the spatial information
- Convolve the filter over the entire input volume
- The filter has the same depth as the input 

Input volume

32x32x3

k activation maps

28x28x1

k filters

# Convolutional layers

- Neurons in an activation map:
- Each neuron is connected to a small region in the input
- All of  neurons in the activation map share parameters
- Receptive field of a neuron
- k filters → k different neurons all looking at the same region in the input volume

k activation maps

28x28x1

<!-- image -->

https://medium.com/@chriskevin\_80184/feature-maps-ee8e11a71f9e 

https://www.youtube.com/watch?v=AgkfIQ4IGaM 

# Convolutional layers Stride

- Stride – the amount by which the filter shifts
- Stride 1

# Convolutional layers Stride

- Stride – the amount by which the filter shifts
- Stride 2

# Convolutional layers Padding

|   0 |   0 |   0 |   0 |   0 |   0 |   0 |
|-----|-----|-----|-----|-----|-----|-----|
|   0 |     |     |     |     |     |   0 |
|   0 |     |     |     |     |     |   0 |
|   0 |     |     |     |     |     |   0 |
|   0 |     |     |     |     |     |   0 |
|   0 |     |     |     |     |     |   0 |
|   0 |   0 |   0 |   0 |   0 |   0 |   0 |

To preserve size spatially:

CONV layers with stride 1, filters of size FxF, and zero-padding with (F-1)/2

Convolve an input of spatial size 7x7 with a 3x3 filter : output spatial size?

Convolve an input of spatial size 7x7 with a 3x3 filter, and applying 0 padding to the input: output spatial size?

padding: 1

# Why convolutions

- Parameter sharing
- A feature detector that’s useful in one part of an image is probably useful in other parts of the image
- Translation invariance
- Sparsity of connections
- The output volume depends only on a small (filter size) subset of the input volume

# Convolutional layers

Padding: “same” or “valid”

<!-- image -->

# Pooling layers

- Operate independently over each  channel in the input
- Reduces the input size, creating smaller (and more manageable) representations
- Common pooling layers:
- max pooling: takes the maximum value within each “patch” in the feature map
- average pooling: takes the average value of each “patch” in the feature map
- It does not contain any learnable weights

# Pooling layers

<!-- image -->

<!-- image -->

Pooling – reduces spatial dimension

Operates independently over each channel

# Pooling layers  Examples

<!-- image -->

<!-- image -->

Max pooling

<!-- image -->

Avg pooling

# Pooling layers Example: max pooling layer, F 2, stride 1

|   1 |   3 |   6 |   6 |
|-----|-----|-----|-----|
|  20 |   9 |   8 |   4 |
|   2 |   1 |   4 |   5 |
|   1 |  12 |  13 |  10 |

|   20 |   9 |   8 |
|------|-----|-----|
|   20 |   9 |   8 |
|   12 |  13 |  13 |

# Pooling layers Example: max pooling layer, F 2, stride 2

|   1 |   3 |   6 |   6 |
|-----|-----|-----|-----|
|  20 |   9 |   8 |   4 |
|   2 |   1 |   4 |   5 |
|   1 |  12 |  13 |  10 |

|   20 |   8 |
|------|-----|
|   12 |  13 |

# Pooling layer

- Parameters
- Filter size (spatial extent): F
- Stride: S
- Input: WI × HI × D
- Output: WO × Hi × D
- It has no learnable parameters
- 

<!-- image -->

<!-- image -->

# Fully connected layers

- They don’t preserve spatial information
- Linear unit followed by a linearity
- Just in regular NN, contain several neurons that are connected to the entire input volume
- Each neuron “sees” the entire input volume

- 

1

DI

DO

1

f(W·X + b)

f – activation function

W – weight matrix (DO × DI)

b – bias vector 

<!-- image -->

- If we have an input volume of 27x27x5, what will be size of this volume if we apply a padding of 2?
- How many parameters (including the bias) does a convolutional layer with 10 filters of size 5x5 have?
- How many parameters (including the bias) does a convolutional layer with 10 filters of size 5x5 have if the input size is 32x32x3? What if we use a stride of 2?
- Given an input volume that is 63x63x16 that is convolved with 32 filters that are each 7x7, using a stride of 2 and no padding. What is the output volume?
- Given a RGB image of size 300x300, and you use a classical neural network with the first hidden layer of 100 neurons (each one fully connected to the input). How many parameters does this hidden layer have?

# Typical neural network architecture

<!-- image -->

Image source: https://medium.com/@RaghavPrabhu/understanding-of-convolutional-neural-network-cnn-deep-learning-99760835f148 

# Typical neural network architecture

- Several CONV, POOL and FC layers stacked together
- [CONV-ReLU-POOL]*N – [FC]*K – softmax
- Recent neural networks change this paradigm and use blocks
- The trend is to reduce the filter sizes and to increase the depth of the networks
- Another trend is to avoid using POOL and FC layers, and use only CONV layers

# CNN architectures

<!-- image -->

14,197,122 images 

1000 categories 

<!-- image -->

2012

<!-- image -->

# Alexnet, 2012

<!-- image -->

A single GTX 580 GPU has only 3GB of memory, which limits the maximum size of the networks

that can be trained on it.

https://proceedings.neurips.cc/paper\_files/paper/2012/file/c399862d3b9d6b76c8436e924a68c45b-Paper.pdf 

# Alexnet, 2012

<!-- image -->

Image size: (227, 227, 3) 

First layer: 96 filter of size 11x11

What is the output volume size and the number of parameter in this layer?

# Alexnet, 2012

<!-- image -->

Image size: (227, 227, 3) 

First layer: 96 filter of size 11x11

Output volume: 55x55x96 

Parameters: (11*11*3)*96 ~ 35K

# Alexnet

<!-- image -->

# Alexnet

<!-- image -->

<!-- image -->

<!-- image -->

# Alexnet – key features

- First use of ReLU
- Overlapped max pooling
- Used normalization layers
- Not used anymore

<!-- image -->

<!-- image -->

RELU (solid line) effect on training

vs tanh (dashed line)

# Alexnet – key features

- Training setup
- Dropout 0.5
- Data augmentation
- Batch size 128
- Gradient descent with momentum (beta = 0.9)
- Initial learning rate: 1e-2, reduced manually, when a plateau was reached
- 7 Alexnet ensemble: 18.2% -&gt; 15.4%

# VGG, 2014

<!-- image -->

https://arxiv.org/abs/1409.1556 

# VGG

- Simplifies the network architectures
- Always use 3x3 filters for convolutions
- Always use max pooling of filter size 2x2 and a stride of 2

# VGG

<!-- image -->

<!-- image -->

- Always use 3x3 filters for convolutions
- Always use max pooling of filter size 2x2 and a stride of 2
- 
- Top 5 error: 11.7 -&gt; 7.3 

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- 

<!-- image -->

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- 

<!-- image -->

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- 

<!-- image -->

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- 

<!-- image -->

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- 

<!-- image -->

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- What is the effect of this replacement in terms on the number of parameters?
- What about non-linearities?
- 

# VGG

- “It is easy to see that a stack of two 3×3 conv. layers has an effective receptive field of 5×5; three such layers have a 7 × 7 effective receptive field.”
- What is the effect of this replacement in terms on the number of parameters?

3*(32C) = 27C vs 72C = 49C

- What about non-linearities?
- 

# VGG – key features

- Did not use local response normalization layers
- Use ensembles to boost performance
- Use VGG 16 or VGG 19 (VGG 19 brings only a small increase in performance, but it requires more memory)
- Similar training procedure as in AlexNet

<!-- image -->

# GoogLeNet, 2014

6.7 top-5 error

<!-- image -->

22 layers

16/19 layers

8 layers

8 layers

https://arxiv.org/abs/1409.4842 

<!-- image -->

<!-- image -->

Inception module

<!-- image -->

This name is an homage to Yann LeCun’s pioneering LeNet 5 network

GoogLeNet, 2014

# Inception module

- You don’t need to ”pick” the filter sizes, instead you let the network “choose” between several values
- Filters of different sizes
- Pooling layer
- Concatenate activations depth-wise

<!-- image -->

# Inception module

<!-- image -->

# Inception module

<!-- image -->

Input: 28x28x256

128

192

96

?

What are the output sizes of all these operations?

# Inception module

<!-- image -->

Input: 28x28x256

128

192

96

28x28x128

28x28x192

28x28x96

28x28x256

# Inception module

<!-- image -->

Input: 28x28x256

128

192

96

28x28x128

28x28x192

28x28x96

28x28x256

28x28x(128+192+96+256) = 28x28x672

# Inception module

<!-- image -->

Input: 28x28x256

128

192

96

28x28x128

28x28x192

28x28x96

28x28x256

28x28x(128+192+96+256) = 28x28x672

[1x1 conv, 128 filters]: ? convolution operations

[3x3 conv, 192 filters]: ? convolution operations

[5x5 conv, 96 filters]: ? convolution operations

# Inception module

<!-- image -->

Input: 28x28x256

128

192

96

28x28x128

28x28x192

28x28x96

28x28x256

28x28x(128+192+96+256) = 28x28x672 = 526848

[1x1 conv, 128 filters]:  28x28x128x1x1x256

[3x3 conv, 192 filters]: 28x28x192x3x3x256

[5x5 conv, 96 filters]:  28x28x96x5x5x256

&gt;850M

# Bottleneck

- Use bottleneck layers to reduce the depth dimensions of the activations
- use 1x1 convolutions

<!-- image -->

# 1x1 convolutions

Each filter has a 1x1xD size and performs a D-dimensional dot product

# 1x1 convolutions

image source : https://medium.com/hitchhikers-guide-to-deep-learning/6-introduction-to-deep-learning-with-computer-vision-3x3-is-a-lie-1x1-convolutions-9edd2baf7fd5

<!-- image -->

# The real Inception module

# The real Inception module

Input: 28x28x256

64

64

64

128

192

96

[1x1 conv, 64]  ??

[1x1 conv, 64]  ??

[1x1 conv, 128] ??

[3x3 conv, 192] ??

[5x5 conv, 96]   ??

[1x1 conv, 64]   ??

# The real Inception module

Input: 28x28x256

64

64

64

128

192

96

[1x1 conv, 64] 28x28x64x1x1x256

[1x1 conv, 64] 28x28x64x1x1x256

[1x1 conv, 128] 28x28x128x1x1x256

[3x3 conv, 192] 28x28x192x3x3x64

[5x5 conv, 96] 28x28x96x5x5x64

[1x1 conv, 64] 28x28x64x1x1x256

358M

&gt;850M

# GoogLe Net

<!-- image -->

Stack Inception modules on top of each other

# GoogLe Net

<!-- image -->

Inception modules

Classifier output removes FC layers

Stem network: 

Conv – Pool – 2x Conv-Pool

# GoogLe Net

<!-- image -->

Output layers to inject additional gradient at lower layers

AvgPool – 1x1 Conv – FC – FC - Softmax

# GoogLe Net

<!-- image -->

Stack Inception modules on top of each other

# GoogLe Net

- Removes fully connected layers
- It was found that a move from fully connected layers to average pooling improved the top-1 accuracy by about 0.6%.

# Global average pooling Network In Network, 2014

- Instead of adding fully connected layers on top of the feature maps, take the average of each feature map, and the resulting vector is fed directly into the softmax layer.
- is more native to the convolution structure by enforcing correspondences between feature maps and categories (feature maps can be easily interpreted as categories confidence maps)
- no parameter to optimize in the global average pooling thus overfitting is avoided at this layer
- sums up spatial information -&gt; more robust to spatial translations of the input 
- 
- 

# Global average pooling (GAP)

<!-- image -->

# GoogLe Net

- 22 layers
- Inception module
- 12x less parameters than AlexNet
- Top 5 error: 6.7%
- Removes fully connected layers as in NIN
- implementation differs in that we use an extra linear layer 

<!-- image -->

<!-- image -->

# ResNet, 2015

6.7 top-5 error

<!-- image -->

22 layers

22 layers

8 layers

8 layers

152 layers

https://arxiv.org/abs/1512.03385 

# ResNet Increasing a network depth

<!-- image -->

Image source: https://towardsdatascience.com/an-overview-of-resnet-and-its-variants-5281e2f56035 

Overfitting? 

# ResNet Increasing a network depth

<!-- image -->

Image source: https://towardsdatascience.com/an-overview-of-resnet-and-its-variants-5281e2f56035 

Overfitting?  NO!

Hypothesis: Deeper models are harder to optimize

# ResNet

- Deeper models shouldn’t hurt performance, they should be at least as good as shallower ones
- Copy the learned layers from the shallower layer and set additional layers to identity mapping

# ResNet

- Try to fit a residual mapping instead of directly trying to fit a desired underlying mapping
- 

<!-- image -->

# Residual blocks

- Residual blocks
- Main path
- Shortcut

<!-- image -->

# ResNet

- Start with a CONV layer
- Stack multiple residual blocks on top of each other
-  Every residual block has two 3x3 conv layers
- Periodically, double number of filters and downsample spatially using stride 2
- No fully connected layers

<!-- image -->

# ResNet &gt; 50 layers

- Use “bottlenecks” just like in Inception networks

<!-- image -->

A deeper residual function F for ImageNet. Left: a building block (on 56×56 feature maps) as in Fig. 3 for ResNet34. Right: a “bottleneck” building block for ResNet-50/101/152.

# ResNet

- Allows us to train really deep networks
- “Identity Mappings in Deep Residual Networks”, 2016 –  trained a 1001-layer deep ResNet to outperform its shallower counterparts
- Helps us with vanishing and exploding gradients
- It is easy for ResNet to learn the identity function
- 

# ResNet

1st places on: 

- ImageNet detection
- ImageNet localization
- COCO detection
- COCO segmentation

<!-- image -->

<!-- image -->

# ResNet

- Batch Normalization after every CONV layer

- He initialization

- Momentum gradient descent (0.9)

- Learning rate: 0.1, divided by 10 when validation error plateaus

- Mini-batch size 256

- Weight decay of 1e-5

- No dropout used

# SENet

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

<!-- image -->

<!-- image -->

<!-- image -->

https://arxiv.org/abs/1709.01507 

# EfficientNet

Presenter: MIHAI-BOGDAN DEACONU

 

https://arxiv.org/abs/1905.11946 