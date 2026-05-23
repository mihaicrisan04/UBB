# Computer Vision and Deep Learning

Semantic segmentation

Lecture 6

# Computer vision tasks

Classification

What object is in this image?

CAT

<!-- image -->

<!-- image -->

Semantic segmentation

What label has each pixel?

Pixel level, we are not interested in objects

https://tariq-hasan.github.io/concepts/computer-vision-semantic-segmentation/ 

# Computer vision tasks

Image source:  https://static.artfido.com/2018/05/cover-12.jpg

<!-- image -->

Object detection: multiple objects

(the label and position - bounding box -  of each object)

Instance segmentation: multiple objects

(the label and position of each object)

# Semantic segmentation Examples

<!-- image -->

<!-- image -->

Autonomous driving

<!-- image -->

# Semantic segmentation Examples

<!-- image -->

Medical image segmentation

https://arxiv.org/pdf/1810.05732.pdf 

https://news.developer.nvidia.com/3d-real-time-video-hair-coloration/ 

Real time hair colouring

<!-- image -->

# Semantic segmentation

- Understating the image at pixel level: each pixel is labelled individually with a class 
- Groups both semantics and location
- Global information: WHAT?
- Local information: WHERE?
- 
- 
- 

<!-- image -->

# Semantic segmentation Naïve approach – sliding window

<!-- image -->

CNN

Classify central pixel

Image patch 

Slide window over the input image and classify each image patch using a CNN

CAT

https://papers.nips.cc/paper/2012/file/459a4ddcb586f24efd9395aa7662bc7c-Paper.pdf 

<!-- image -->

# Semantic segmentation Naïve approach – sliding window

<!-- image -->

CNN

Classify central pixel

Image patch 

Slide window over the input image and classify each image patch using a CNN

CAT

Highly inefficient!!

# Let’s design a CNN to classify all pixels at once!

- How would we encode the output (we need to output a class label for each pixel)? 
- What would be the input and the output size of this network?

Fully convolutional neural networks

# Fully convolutional neural networks

<!-- image -->

FCN 

W

H

W

H

C

Output depth:

Number of classes 

The output should have the same spatial size as the input

# https://www.jeremyjordan.me/semantic-segmentation/#loss  

<!-- image -->

<!-- image -->

# Output layer

|   2 |   1 |
|-----|-----|
|   1 |   0 |

|   0 |   0 |
|-----|-----|
|   0 |   1 |

Ground truth label image

3 classes (0, 1, 2)

One hot encodings

2 – [0, 0, 1]

1 – [0, 1, 0]

0 – [1, 0, 0]

|   0 |   1 |
|-----|-----|
|   1 |   0 |

|   1 |   0 |
|-----|-----|
|   0 |   0 |

Class 0

Class 1

Class 2

Apply cross entropy pixel-wise on the last layer of the network

# Semantic segmentation metrics

# Segmentation metrics

<!-- image -->

# Segmentation metrics

# Segmentation metrics

# Segmentation metrics Pixel Accuracy (PA)

# Semantic segmentation metrics

Pixel accuracy

- Number of pixels classified correctly by the network

Ground Truth

Prediction

https://ilmonteux.github.io/2019/05/10/segmentation-metrics.html

# Segmentation metrics Precision and recall per class

# Segmentation metrics Intersection over union (IoU)

# Segmentation metrics Intersection over union (IoU)

# Segmentation metrics Intersection over union (IoU)

# Segmentation metrics Mean IoU

# Segmentation metrics Frequency Weighted mean IoU (FWIoU)

- 
- 
- A mIoU that weights each class by its pixel frequency in the ground truth.
- Large classes (many pixels) contribute more. 
- If background occupies 80% of pixels, it contributes 80% of FWIoU.
- Use it when you care more about getting the large structures right. 
- E.g. In autonomous driving, correctly segmenting the road (large) may matter more than a distant traffic sign (small).

# Segmentation metrics Dice Coefficient (F1 score)

# Segmentation metrics Dice Coefficient (F1 score)

# Segmentation metrics Dice Coefficient (F1 score)

- Dice is always ≥ IoU for the same prediction
- The mapping is monotonic: if model A has higher IoU than model B, it also has higher Dice, and vice versa. 
- Rankings don’t change!
- the choice is largely convention
- medical imaging uses Dice (historical, aligns with the Dice loss). CV benchmarks use IoU (Pascal VOC tradition).
- Dice is numerically higher, which can make results look better! Be aware when comparing methods.

# Semantic segmentation metrics

<!-- image -->

https://ilmonteux.github.io/2019/05/10/segmentation-metrics.html 

<!-- image -->

# Segmentation metrics Boundary IoU

- Boundary quality is often important in image segmentation
- Boundary IoU focuses evaluation on the contour region. 

https://arxiv.org/abs/2103.16562 

<!-- image -->

# Segmentation metrics Boundary IoU

https://arxiv.org/abs/2103.16562 

<!-- image -->

# Segmentation metrics Boundary IoU

https://arxiv.org/abs/2103.16562 

<!-- image -->

- IoU can be high even with sloppy boundaries if the object is large (a few misaligned edge pixels barely change the ratio)
- BIoU looks only on the boundary and ignores the easy interior. It measures contour quality.
- Report both:
- IoU captures region quality
-  BIoU captures edge quality. 
- A model might have high IoU but low BIoU (good coverage, fuzzy edges) or high BIoU but low IoU (sharp edges but topological errors elsewhere).

# Segmentation metrics Boundary IoU

https://arxiv.org/abs/2103.16562 

# Semantic segmentation losses

# Semnatic segmentation losses

- Pixel-wise Cross-Entropy (CE)
- Weighted Cross-Entropy (WCE)
- Focal Loss:

# Semnatic segmentation losses

# Semnatic segmentation losses

- Pixel-wise Cross-Entropy (CE)
- Weighted Cross-Entropy (WCE)
- Introduce a per-class weight to amplify the contribution of rare classes.
- Amplify the gradient signal from rare classes. 

- 

# Semnatic segmentation losses

- Pixel-wise Cross-Entropy (CE)
- Weighted Cross-Entropy (WCE)
- Focal Loss:
- Down-weights easy pixels so the model focuses on hard ones.
- Originally for object detection, and then adopted in segmentation.
- 

# Semnatic segmentation losses

# Semnatic segmentation losses Dice Loss

predicted volume + ground truth volume

# Semnatic segmentation losses Dice Loss

# Semnatic segmentation losses Generalized Dice Loss (GDL)

# Semnatic segmentation losses Generalized Dice Loss

# Semnatic segmentation losses Generalized Dice Loss

# Semnatic segmentation losses Generalized Dice Loss

# Semnatic segmentation losses Generalized Dice Loss

# Semnatic segmentation losses Tversky loss

# Semnatic segmentation losses Tversky loss

# Semnatic segmentation losses Compound losses

# Architectures for semantic segmentation

# Fully convolutional neural networks (FCN)

- How to preserve the spatial size of the output map (input size must be equal to the output size)?

# Fully convolutional neural networks

- How to preserve the spatial size of the output map (input size must be equal to the output size)?
- Don’t use any layers that reduce the spatial size 

# Fully convolutional neural networks

- How to preserve the spatial size of the output map (input size must be equal to the output size)?
- Don’t use any layers that reduce the spatial size -&gt; inefficient
- Use two paths in the network: a down-sampling and an up-sampling path

# Fully convolutional neural networks

<!-- image -->

<!-- image -->

Down sampling  layers

Up-sampling layers

<!-- image -->

Down-sampling: strided convolutions, pooling layers

Up-sampling: NN, bilinear interpolation, max unpooling, transposed convolution

# Up-sampling techniques

# Up-sampling Nearest neighbour

|   4 |   5 |
|-----|-----|
|  10 |  20 |

|   4 |   4 |   5 |   5 |
|-----|-----|-----|-----|
|   4 |   4 |   5 |   5 |
|  10 |  10 |  20 |  20 |
|  10 |  10 |  20 |  20 |

2x2

4x4

# Up-sampling Bilinear interpolation

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# Up-sampling Bilinear interpolation

|   10 |   20 |
|------|------|
|   30 |   40 |

|   10 |   12.5 |   17.5 |   20 |
|------|--------|--------|------|
|   15 |   17.5 |   22.5 |   25 |
|   25 |   27.5 |   32.5 |   35 |
|   30 |   32.5 |   37.5 |   40 |

2x2

4x4

# Up-sampling in torch

Interpolation:

 - nearest

 - bilinear

 - bicubic

 - trilinear 

<!-- image -->

# Transposed convolutions Learnable up-sampling

Remember strided convolutions

3x3, stride 2, pad 1

# Transposed convolutions Learnable up-sampling

Remember strided convolutions

3x3, stride 2, pad 1

# Transposed convolutions Learnable up-sampling

Remember strided convolutions

3x3, stride 2, pad 1

# Transposed convolutions Learnable up-sampling

Remember strided convolutions

3x3, stride 2, pad 1

# Transposed convolutions Learnable up-sampling

NOW: transposed convolutions

| i1   |    |
|------|----|
|      |    |

3x3, stride 2, pad 1

filter

The input feature i1 is multiplied with the filter values

i1 is like a weight for the filter value. 

Copy the “weighted” filter in the corresponding output

# Transposed convolutions Learnable up-sampling

NOW: transposed convolutions

|    | i2   |
|----|------|
|    |      |

3x3, stride 2, pad 1

filter

The input feature i2 is multiplied with the filter values

i2 is like a weight for the filter value. 

Copy the “weighted” filter in the corresponding output

Where there is overlap, sum up the values

# Transposed convolutions Learnable up-sampling

The weights of the filter are learned

At each position multiply the filter values with the corresponding position of the input layer to get the result of the output

Add overlapping positions

Example: 1 stride and 0 padding

<!-- image -->

<!-- image -->

# Transposed convolutions Learnable up-sampling

<!-- image -->

Image source: https://medium.com/apache-mxnet/transposed-convolutions-explained-with-ms-excel-52d13030c7e8 

# Transposed convolutions in torch

<!-- image -->

# Convolutional neural networks for semantic segmentation

# U-Net: Convolutional Networks for Biomedical Image Segmentation 

- https://arxiv.org/abs/1505.04597 

# U-Net, 2015

<!-- image -->

Skip connections to help the upsampling path recover spatially detailed information from the downsampling path

# Rethinking Atrous Convolution for Semantic Image Segmentation 

- https://arxiv.org/abs/1706.05587  

- Semantic segmentation requires assigning a class label to every single pixel in an image
- large receptive fields for global context 
- high resolution for precise pixel-level boundaries
- Standard CNNs for classification use repeated pooling/striding, which progressively shrinks spatial resolution
- Upsampling coarse feature maps produces blurry, imprecise segmentation masks
- Idea: capture multi-scale context while maintaining dense, high-resolution feature maps

# DeepLabV3

https://arxiv.org/abs/1802.02611 

# DeepLabV3 Atrous (Dilated) Convolution

https://arxiv.org/abs/1802.02611 

- Standard convolution applies a dense kernel to adjacent pixels
- Atrous convolution inserts zeros ("holes") between kernel weights, expanding the receptive field without adding parameters

<!-- image -->

# DeepLabV3 Atrous (Dilated) Convolution

https://arxiv.org/abs/1802.02611 

- Uses a dilation rate: r (spacing between kernel elements)
- Effective kernel size formula: k\_eff = k + (k − 1)(r − 1); k – kernel size
- 3×3 filter, rate=1 is standard convolution
- 3×3 filter, rate=2 gives 5×5 effective filter size
- 3x3 filter, rate=4 gives 7×7 effective filter size
- 

<!-- image -->

# DeepLabV3 Atrous (Dilated) Convolution

https://arxiv.org/abs/1802.02611 

- Same number of parameters parameters regardless of rate
- Advantage: enlarges the field of view without reducing feature map resolution or increasing computation
- By running multiple rates in parallel, we extract features at multiple scales simultaneously at the same spatial resolution
- 

# DeepLabV3 Atrous (Dilated) Convolution

https://arxiv.org/abs/1802.02611 

# DeepLabV3 Atrous (Dilated) Convolution

https://arxiv.org/abs/1802.02611 

- How many of the 9 kernel weights actually land on valid (real) data as you increase the dilation rate?
- past a certain point, the atrous convolutions collapse into pointwise operations and stop capturing any spatial relationships
- use a global average pooling branchto  solve this by capturing whole-image context

<!-- image -->

# DeepLabV3 Architecture 

https://arxiv.org/abs/1802.02611 

- DeepLabV3 uses a standard classification network (ResNet-50, ResNet-101, or modified Xception) as feature extractor
- The last blocks have their stride removed and replaced with atrous convolutions
- controlled "output stride" : the ratio of input resolution to final feature map resolution
- E.g. Output stride = 16: input 513×513 produces 33×33 feature maps (default for training; faster, less memory)
- E.g. Output stride = 8: input 513×513 produces 65×65 feature maps (used at inference for higher accuracy; 4× denser)
- Instead of using a single dilation rate for all three convolutional layers in a residual block, each layer gets a different rate
- Multi Grid = (r1, r2, r3) the unit rates for the three convolutional layers within block4 to block7
- Multi-grid rates are defined as a tuple,  multiplied by the block's base dilation rate
- Example: if block base rate = 2 and multi-grid = (1, 2, 4), the three layers use rates (2, 4, 8)

# DeepLabV3 

https://arxiv.org/abs/1802.02611 

- Output stride -  ration between input resolution and feature map resolution

<!-- image -->

<!-- image -->

# DeepLabV3 

https://arxiv.org/abs/1802.02611 

- Output stride -  ration between input resolution and feature map resolution

Conv1 + Pool1- output stride 4. 

Block1 - output stride 8

Block2 - output stride 16, and Block3 stays at stride 16

Block3 – onwards: output stride = 16 and use atrous convolution with dilation rate 2 to maintain the same resolution, but growing the receptive field.

# DeepLabV3 

https://arxiv.org/abs/1802.02611 

- Output stride -  ration between input resolution and feature map resolution

<!-- image -->

ASPP

- 5 parallel branches:
- (a) conv 1x1 – local, no spatial context, 3 3x3 atrous convolutions at rates 6 (nearby spatial context ), 12 (medium-range context), 18 (broad spatial extent)
- (b) GAP: global context

Fusion: All five outputs are concatenated along the channel dimension &amp; projected back to 256 channels through a 1×1 convolution.

- Five parallel branches process the backbone's output feature map simultaneously: 
1. 1×1 convolution (captures local, per-pixel features)
2. 3×3 atrous convolution with rate = 6
3. 3×3 atrous convolution with rate = 12
4. 3×3 atrous convolution with rate = 18
5. Global average pooling → 1×1 conv → bilinear upsample (image-level features)
- provides holistic scene understanding
- Each branch outputs 256 channels with batch normalization and ReLU
- All five outputs are concatenated (1280 channels total) and fused through a final 1×1 convolution back to 256 channels

# DeepLabV3 Atrous Spatial Pyramid Pooling (ASPP) 

https://arxiv.org/abs/1802.02611 

- ASPP module produces a 256-channel feature map at 1/16th (or 1/8th) of the input resolution
- 1×1 convolution maps it down to num\_classes channels (e.g., 21 for PASCAL VOC)
- this prediction map is bilinearly upsampled by a factor of 16× (or 8×) back to the original image size

# DeepLabV3 Final prediction 

https://arxiv.org/abs/1802.02611 

# Deep High-Resolution Representation Learning for Visual Recognition

- https://arxiv.org/pdf/1908.07919

# HRNet

- maintain high-resolution representations through the segmentation process 
- connect the high-to-low resolution convolution streams in parallel
- Repeatedly exchange the information across resolutions
- result: a semantically richer and spatially more precise representation
- 

# HRNet

- stem network: 2 3x3 convolutions decreasing the resolution to ¼
- main body (below)
- parallel multi-resolution convolutions
- multi-resolution fusion
- final representation head
- 

<!-- image -->

# HRNet – parallel multi-resolution

- gradually add high-to-low resolution streams to the architecture
- the resolutions for the parallel streams of a later stage consists of the resolutions from the previous stage, and an extra lower one
- 
- 
- 
- 
- Nsr s – stage, r – resolution index (r = 1 for the first stream)
- r = 1/2r−1 of the resolution of the first stream

<!-- image -->

# HRNet – multi-resolution fusion

- Goal: exchange the information across multi-resolution representations
- 
- 

<!-- image -->

# HRNet – multi-resolution fusion

<!-- image -->

# HRNet – multi-resolution fusion

<!-- image -->

# HRNet – multi-resolution fusion

<!-- image -->

# HRNet – multi-resolution fusion

# HRNet – multi-resolution fusion

<!-- image -->

# HRNet – multi-resolution fusion

# HRNet – multi-resolution fusion

<!-- image -->

# HRNet – representation head

- three kinds of representation heads
- HRNetV1: use only the output of the high resolution stream
- HRNetV2: rescale the low-resolution representations through bilinear upsampling to the high resolution, concatenate, followed by a 1 × 1 convolution to mix the four representations; used in sem. segmentation
- HRNetV2p: multi-level representations by downsampling the high-resolution representation output from HRNetV2 to multiple levels
- 
- 

# HRNet – representation head

- three kinds of representation heads
- HRNetV1
- HRNetV2
- HRNetV2p
- 
- 

<!-- image -->

# Transformer based architectures for semantic segmentation

# Vision transformer for dense predictions

- https://arxiv.org/pdf/2103.13413 

# DPT

- encoder-decoder structure
- Encoder: uses ViT transformer as the basic computational building block
- Convolutional decoder: reassemble the ViT’S representation into image-like feature representations at various resolutions, and progressively combine them into the final dense prediction

<!-- image -->

# DPT - encoder

- Uses positional encodings
- Still includes CLS token (readout token): global image representation
- 3 encoder versions:
- ViTBase: 12 transformer layers
- ViTLarge: 24 transformer layers
- ViT Hybird: Resnet50 + 12 transformer layers
- 
- 

<!-- image -->

# DPT - decoder

<!-- image -->

Assembles the set of tokens into image-like feature representations at various resolutions, which are progressively fused into the final dense prediction. It is based on a three-stage Reassemble operation.

# DPT - Reassemble

<!-- image -->

# DPT – Reassemble (Read)

- Read – map Np + 1 tokens to a set of Np tokens that can be concatenated into an image-like representation
- Several variants

- 
- 

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# DPT – Reassemble (Concatenate)

- Concatenate: reshape tokens to an image-like representation by placing each token according to the position of the initial patch in the image (spatial concatenation operation)
- 
- 
- 
- p – patch size (p = 16)

<!-- image -->

# DPT – Reassemble (Resample)

<!-- image -->

# DPT - Fusion

- Features are reassembled at four different stages and four different resolutions
- applies convolutions for input adaptation (same feature)
- up-samples smaller feature maps (half the resolution of the input image)
- project

<!-- image -->

# DPT – Segmentation results

<!-- image -->

# SegFormer: Simple and Efficient Design for Semantic Segmentation with Transformers

- https://arxiv.org/pdf/2105.15203.pdf

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description 

# SegFormer

- Transformer for semantic segmentation
- positional-encoding-free 
- hierarchical encoder
- all-MLP decoder design that yields a powerful representation
- 

<!-- image -->

# SegFormer: architecture

1. hierarchical transformer encoder to generate high-resolution coarse features and low-resolution fine feature
2. a lightweight All-MLP decoder to fuse these multi-level features to produce the final semantic segmentation mask

# SegFormer

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

<!-- image -->

# SegFormer

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

Encoder: input image is scaled to its 1/32 size

Decoder:  upsampled to 1/4 of the original size

<!-- image -->

# SegFormer: hierarchical encoder

- generates CNN-like multi-level features (both high-resolution coarse features and low-resolution fine-grained)
- given an input image with a resolution of HxWx3, patch merging is performed to obtain a hierarchical feature map Fi with a resolution of H/(2i+1)xW/(2i+1)x Ci
- where i ∈ {1, 2, 3, 4}, Ci &gt; Ci+1

# SegFormer Encoder

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

Overlaped Patch Embedding

- Converts image patches into tokens
- Unlike ViT uses overlapping patches
- patch size &gt; stride -&gt;  information sharing between patches
- features don't abruptly cut off at patch edges
- K (patch size), S (stride), and P (padding size), S &lt; K
- K = 7, S = 4, P = 3 and K = 3, S = 2,P = 1 to perform overlapping patch merging to produces features with the same size as the non-overlapping process.

<!-- image -->

# SegFormer Encoder 

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

SegFormer Block – reduce the complexity of the attention mechanism

reduction ratio R to reduce the length of the sequence (N = H x W)

Key and value go through a reduction layer

<!-- image -->

Linear layer with CꞏR input and generating C dimensional output

<!-- image -->

# SegFormer Encoder 

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

Mix FFN 

- Instead of adding positional encodings, SegFormer encodes spatial awareness directly into the FFN using a 3×3 depthwise 2D convolution
- Tokens are first reshaped back into a 2D grid
- A token's output depends on its neighbors and no two positions in an image have exactly the same neighborhood pattern -&gt; distinguish positions through context
- 

<!-- image -->

# SegFormer Encoder 

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

Overlap Patch Merging

- Between encoder stages, the feature map needs to be downsampled and increased depth
- Tokens are reshaped to 2D before any operation
- Stride=2 halves spatial resolution: Downsamples the spatial resolution between stages (1/4 → 1/8 → 1/16 → 1/32 of input)
- K=3 &gt; S=2 preserves boundary context between neighboring tokens
- Cᵢ changed to Cᵢ₊₁ expands channel depth, encoding richer semantics
- Result is re-flattened into a new, shorter token sequence for the next stage

# SegFormer decoder

- lightweight decoder (MLP layers) 
- MLP layer to unify the channel dimension
- features are up-sampled to 1/4th image size and concatenated together
- MLP layer to fuse the concatenated features
- MLP layer for the final classification
- 

<!-- image -->

# SegFormer decoder

https://github.com/ACSEkevin/An-Overview-of-Segformer-and-Details-Description

1. MLP to bring to same channel dimension
2. Upsample to H/4 x W/4

<!-- image -->

predict the final segmentation mask

# FOUNDATION MODELS FOR SEMNATIC SEGMENATION*

https://segment-anything.com/ 

*Studied in the next lecture