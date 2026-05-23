# Computer Vision and Deep Learning

Lecture 7

Object detection models

# Object localization

- What object is this image and where is this object located in the image?

<!-- image -->

CAT

2

# Object localization

<!-- image -->

Image source: https://leonardoaraujosantos.gitbook.io/artificial-inteligence/machine\_learning/deep\_learning/object\_localization\_and\_detection 

3

# Object localization

<!-- image -->

Image source: https://leonardoaraujosantos.gitbook.io/artificial-inteligence/machine\_learning/deep\_learning/object\_localization\_and\_detection 

C class scores

Softmax loss Ls

BBOX coordinates

4 outputs (x, y, w , h)

L2 loss L2

4

# Object detection

5

<!-- image -->

# Object detection

- Determine the class (label) and the position of EACH object in the input image
- We can’t use the same approach as for localization
- Object detection challenges?
- Sliding window approach?

6

# Object detection challenges

- Multiple outputs: Need to output variable numbers of objects per image
- Multiple types of output: Need to predict ”what” (label) as well as “where” (bounding box)
- Large images: Classification works at 224x224; need higher resolution for detection, &gt;800x600

7

# Object detection metrics

IoU &gt; 0.5 – decent

IoU &gt; 0.7  - acceptable

Iou &gt; 0.9 – almost perfect

9

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

Use IOU to determine is the object is a TP, FP, or a FN

Remember precision and recall?

10

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

Use IOU and a threshold to determine is the object is a TP, FP, or a FN

Remember precision and recall?

<!-- image -->

11

# mAP

- Run object detector on the entire test set
- For each category, compute Average Precision (AP) = area under Precision vs Recall Curve 
- For each detection (sorted from highest score to lowest score) 
- If it matches a GT box with IoU &gt; threshold, mark it as positive and eliminate the GT 
- Else mark it as negative 
- Compute precision and recall
- Add a point on the PR Curve
- Average Precision (AP) = area under PR curve
- Mean Average Precision (mAP) = average of AP for each category

12

# mAP

13

<!-- image -->

# Evaluation metrics for object detection AP

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

Average Precision (AP) is finding the area under the precision-recall curve.

confidence

14

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

TP?

FP?

Precision?

Recall?

<!-- image -->

15

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

TP = 1

FP = 0

Precision = 1/1

Recall = 1/5=0.2

<!-- image -->

16

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

TP = 2

FP = 0

Precision = 2/2

Recall = 2/5=0.4

<!-- image -->

TP+FN -  the total number of samples that are positive (ground truth annotation)

17

# Evaluation metrics for object detection

Images and notes from: https://jonathan-hui.medium.com/map-mean-average-precision-for-object-detection-45c121a31173  

<!-- image -->

TP = 2

FP = 1

Precision = 2/3

Recall = 2/5=0.4

<!-- image -->

18

# Evaluation metrics for object detection

19

# Evaluation metrics for object detection

<!-- image -->

20

# Evaluation metrics for object detection

- For “COCO mAP”: Compute mAP@thresh for each IoU threshold (0.5, 0.55, 0.6, …, 0.95) and take the average

21

# Object detectors

Object Detection in 20 Years: A Survey 

22

# Proposal based object detection

# R-CNN Region-based Convolutional Network, 2014

- Idea:
- Use an algorithm (or network) to find the regions of interests (ROIs) that are likely to contain an object
- Localize (label + bounding box) localize the object in each in region

https://arxiv.org/pdf/1311.2524.pdf 

<!-- image -->

24

# R-CNN Region-based Convolutional Network, 2014

Image source:  https://towardsdatascience.com/r-cnn-fast-r-cnn-faster-r-cnn-yolo-object-detection-algorithms-36d53571365e

<!-- image -->

Selective search to extract 2000 regions. NO learning here

25

# R-CNN Region-based Convolutional Network, 2014

Image source:  https://towardsdatascience.com/r-cnn-fast-r-cnn-faster-r-cnn-yolo-object-detection-algorithms-36d53571365e

<!-- image -->

Selective search to extract 2000 regions. NO learning here

Warp each image region into a square of 227x227  ROI (will be fed to AlexNet)

26

# R-CNN Region-based Convolutional Network, 2014

Image source:  https://towardsdatascience.com/r-cnn-fast-r-cnn-faster-r-cnn-yolo-object-detection-algorithms-36d53571365e

<!-- image -->

Selective search to extract 2000 regions. NO learning here

Warp each image region into a square of 227x227  ROI (will be fed to AlexNet)

AlexNet – feature extractor

27

# R-CNN Region-based Convolutional Network, 2014

Image source:  https://towardsdatascience.com/r-cnn-fast-r-cnn-faster-r-cnn-yolo-object-detection-algorithms-36d53571365e

<!-- image -->

Selective search to extract 2000 regions. NO learning here

Warp each image region into a square of 227x227  ROI (will be fed to AlexNet)

AlexNet – feature extractor

SVMs to classify the presence + label of an object in region

Regression to fine tune the bounding boxes

28

# BBox regression

- Consider a region proposal with center (px, py) and size  (pw, ph)
- RCNN predicts a transform (tx, ty, tw, th) to correct the region proposal

<!-- image -->

29

# BBox regression

Ouput bbox:

bx = px + pw·tx

by = py + ph·ty

bw = pw · exp(tw)

bh = ph · exp(th)

The position is shifted by an amount relative to proposal size 

Scale the size; exp ensures that scaling factor is &gt; 0

The transform encodes relative difference between proposal and output (CNN doesn’t see absolute size or position after cropping)

<!-- image -->

30

# BBox regression

Ouput bbox:

bx = px + pw·tx

by = py + ph·ty

bw = pw · exp(tw)

bh = ph · exp(th)

Solving for the transform given the proposal and target:

tx = (bx - px)/pw

ty = (by - py)/ph

tw = log(bw/pw)

th = log(bh/ph)

<!-- image -->

31

# R-CNN

- Running time: “13s/image on a GPU or 53s/image on a CPU”

<!-- image -->

32

# Fast RCNN

- Idea: feed to image only once to the CNN to extract a feature map, then crop and warp regions of this feature map

<!-- image -->

33

# Fast R-CNN

<!-- image -->

Feed the ENTIRE image only once though the CONV net

34

# Projecting points and boxes

- Receptive fields
- Moving one unit in the output space also moves the receptive field by one
- There is a correspondence between the coordinate system of the input and the coordinate system of the output

35

# Fast R-CNN

<!-- image -->

Extract the ROIs (proposed by selective search) at feature map level using RoI Pooling

36

# Fast R-CNN

<!-- image -->

Feed the ENTIRE image only once though the CONV net

Extract the ROIs (proposed by selective search) at feature map level using RoI Pooling

Bounding boxes

(x, y, w, h)

N classes  + background

37

# Roi Pooling

<!-- image -->

“Snap” to grid cells

Divide the H × W ROI window into an h × w grid of sub-windows of approximate size H/h × W/w and then max-pool the values in each sub-window into the corresponding output grid cell.

Example: H = 4, W = 6, h = 3, 2 = 3

38

# Roi Pooling

<!-- image -->

W = 6, H = 4

w = 3, h = 3

szw = 6/3=2, szh=4/3 = 1

39

# Roi Pooling

<!-- image -->

W = 6, H = 4

w = 3, h = 3

szw = 6/3=2, szh=4/3 = 1

Apply max pooling in each ROI

40

# ROI Pooling example

https://towardsdatascience.com/understanding-region-of-interest-part-1-roi-pooling-e4f5dd65bb44 

https://towardsdatascience.com/understanding-region-of-interest-part-2-roi-align-and-roi-warp-f795196fc193 

<!-- image -->

41

# ROI Pooling

- Region features always the same size even if input regions have different sizes! 

42

# Fast RCNN

<!-- image -->

What is the most time consuming part of Fast RCNN?

43

# Faster-RCNN

<!-- image -->

Unified framework for object detection

Region proposal network:

replaces selective search  and predicts regions directly from feature maps

The rest of the network is similar to Fast RCNN: use ROI pooling to crop feature maps and classify each region

44

# Region proposal newtork

<!-- image -->

<!-- image -->

Run image through backbone CNN (features are aligned to input image)  -&gt; Each feature corresponds to a point in the input

Anchor box of fixed size at each position of the feature map

3x480x640

512x5x6

45

# Region proposal newtork

<!-- image -->

<!-- image -->

Run image through backbone CNN (features are aligned to input image)  -&gt; Each feature corresponds to a point in the input

Anchor box of fixed size at each position of the feature map

  - for each anchor: 

	- is it an object or not?

	- bbox transformation

3x480x640

512x5x6

2x5x6

4x5x6

46

# Region proposal newtork

<!-- image -->

<!-- image -->

Run image through backbone CNN (features are aligned to input image)  -&gt; Each feature corresponds to a point in the input

Anchor box of fixed size at each position of the feature map

  - for each anchor: 

	- is it an object or not?

	- bbox transformation

3x480x640

512x5x6

K anchor boxes

2Kx5x6

4Kx5x6

47

# Region proposal network

- takes an image (of any size) as input and outputs a set of rectangular object proposals, each with an objectness score

<!-- image -->

An anchor box of fixed size at each point in the feature map

- For each anchor box predict if there is an object inside it
- For “positives” predict corrections for the bounding boxes

48

# Faster RCNN

- Multi-loss training
- Region proposal network: objectness classification
- Region proposal network: bbox coordinates
- Final classification score
- Final bbox coordinates
- 

<!-- image -->

49

# Faster RCNN  Loss

<!-- image -->

<!-- image -->

<!-- image -->

t* - ground truth

t –prediction

ha , wa – anchor box 

bounding-box regression from an anchor box to a nearby ground-truth box.

50

<!-- image -->

51

<!-- image -->

52

# Instance segmentation

# Mask R-CNN

<!-- image -->

Also predict a 14x14 mask for each of the C classes (semantic segmentation mini-net)

Just like Faster-RCNN

54

# ROI align

- Cropping features using RoI Pooling: “snap” to grid cells
- Cropping features using RoI Align: No “snapping”
- Divide into equal-sized subregions (may not be aligned to grid!)
- Sample features at regularly-spaced points in each subregion using bilinear interpolation
- Use average pooling

<!-- image -->

55

<!-- image -->

56

# Mask RCNN Beyond instance segmentation

<!-- image -->

One mask for each of the K different keypoints

Each ground truth mask has a single pixel turned on

<!-- image -->

57

# Mask RCNN Beyond instance segmentation

<!-- image -->

58

# One stage object detection

# YOLO You only look once

- Detection is modelled as a regression problem
- image is divided into an S × S grid 
-  for each grid cell predict:
- B bounding boxes (4 values per bbox)
- confidence for those boxes,
- C class probabilities

60

<!-- image -->

61

# YOLO Anchor boxes

- Allows the object detector to specialize better (detect “thinner” or “wider” objects)

https://www.youtube.com/watch?v=RTlwl2bv0Tg 

<!-- image -->

<!-- image -->

62

# YOLO Example with two anchor boxes and 3 classes, S = 3

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

63

<!-- image -->

For evaluating YOLO on PASCAL VOC, we use S = 7, B = 2. PASCAL VOC has 20 labelled classes so C = 20. Our final prediction is a 7 × 7 × 30 tensor

64

# YOLO Non maxima suppression (NMS) 

- Discard all predictions with proba &lt;= threshold
- while there are any remaining boxes
- Select the box with the largest proba (maxBB) as a prediction
- Discard boxes with IoU &gt;= 0.5 with maxBB

<!-- image -->

https://www.youtube.com/watch?v=VAo84c1hQX8 

<!-- image -->

65

# YOLO Loss function

<!-- image -->

This is the loss function for yolo v1 paper.

 Here each bounding box predicts an objectness score and a 4 coordinates, but the class predictions are per cell. Hence this: 

66

# YOLO – making predictions

1.Run CNN on input images (example: S = 3, B = 2)

<!-- image -->

67

# YOLO – making predictions

1.Run CNN on input images (example: S = 3, B = 2)

2. Remove low predictions

<!-- image -->

68

# YOLO – making predictions

1.Run CNN on input images (example: S = 3, B = 2)

2. Remove low predictions

3. NMS

<!-- image -->

69

# Yolo v5

- Backbone: extract rich feature representation for images. 
- Model Neck: extract feature pyramids (generalize to objects on different sizes and scales).
- Model Head: perform the final stage operations. It applies anchor boxes on feature maps and produce the final output: classes , objectness scores and bounding boxes.
- 

<!-- image -->

70

# Yolo v5

<!-- image -->

-  Backbone: CSP-Darknet53 
- Neck: SPPF and PANet 
- Head: similar to head used in YOLOv4.

71

# Yolo v5 - backbone

<!-- image -->

72

# Yolo v5 - backbone

- C3 block: CSP layer with 3 convolutions (C3)

<!-- image -->

Yolo v4

<!-- image -->

<!-- image -->

Yolo v5

73

# Yolo v5 – backbone (activation function)

<!-- image -->

SiLU (Sigmoid Linear Unit/swish activation function) is used with the convolution

74

# Yolo v4 – SPP (Spatial Pyramid Pooling)

- Pass the input image through several convolutional layers and then apply max pooling layers with different kernel sizes
- Concatenate result to get a fixed length representation
- Purpose: identify objects at different sizes

<!-- image -->

75

# Yolo v5 – SPPF (Spatial Pyramid Pooling Fast)

Instead of using parallel max pooling layers with different filter sizes, use the same filter size (5x5) and use sequential max polling layers

Conv

Concat

Conv

Max Pool

Max Pool

Max Pool

SPPF – Spatial Pyramid Pooling Fast

76

# Yolo v4 – PANet (Path Aggregation Network)

- PANet is a feature pyramid network used to improve information flow and help in the proper localization of pixels in the task of mask prediction.
- Combines features from different levels 

<!-- image -->

<!-- image -->

FPN

Top down part

Bottom up part

77

# Yolo v4 – PANet (Path aggregation network)

Top down part

Bottom up part

Shortcut connections

78

# Yolo v5 – PANet (Path Aggregation Network)

Integrate CSP blocks in the Path Aggregation network

<!-- image -->

79

# Yolo v5 – detection head

- Use the same detection head as yolo v3 and yolo v4
- Predicts the location of the bounding boxes (x, y, height, width), the scores and the objects classes.

80

# Yolo v5 – detection head

<!-- image -->

Reshape

Transpose

Sigmoid

Split

Mult (2)

Add

Mult (stride)

Concat

Mult (2)

Pow(2)

Mult (A)

Reshape

Categories

1x255x40x40

1x3x85x40x40

1x3x40x40x85

1x3x40x40x2

1x3x40x40x2

1x3x40x40x2

Categories

1x3x40x40x81

1x4800x85

x,y

Grid cell coords

Multiply with anchor boxes

<!-- image -->

81

# Yolo v5 – detection head

- The network outputs raw offsets tx and ty​ (one for each anchor cell)
- Apply sigmoid to scale the offset in the range [0, 1]
- Scale and shift so the predicted center can move slightly beyond its grid cell
- (cx, cy) – cell corners
- Multiplying by 2 stretches σ(t) into (0,2)
- Subtracting 0.5 shifts it to (−0.5, 1.5)
- Allow the box center to be up to 0.5 cells outside the cell boundaries
- 

<!-- image -->

<!-- image -->

82

# Yolo v5 – detection head

- The network outputs raw offsets tw and th​ (one for each anchor cell)
- Apply sigmoid, then stretch &amp; square to ensure a strictly positive scaling factor
- scalew​=(2σ(tw​))2
- scaleh​=(2σ(th​))2
- (t)∈(0,1)→2 σ(t)∈(0,2): squaring yields (0,4). Scale is positive and can “grow” the anchor up to 4× its base size.
- Multiply by the predefined anchor dimensions (pw, ph)

<!-- image -->

<!-- image -->

83

# DETR: End-to-End Object Detection with Transformers

https://github.com/facebookresearch/detr 

# DETR

- Simplifies the process of object detection by formulating object detection as a direct set prediction problem
- Previous methods make predictions based on some initial guesses
- Two stage methods predict objects w.r.t. proposals
- Single stage methods predict objects w.r.t. a grid of possible object centers
- DETR reasons about the relations of the objects and the global image context to directly output the final set of predictions in parallel

85

# DETR

<!-- image -->

DETR infers a fixed-size set of N predictions, in a single pass through the model, where N is significantly larger than the typical number of objects in an image

86

# DETR

<!-- image -->

Starting from the initial image, a CNN backbone is used to extract a compact feature representation with a lower resolution feature map 

<!-- image -->

<!-- image -->

<!-- image -->

CNN

87

# DETR

<!-- image -->

- 1x1 convolution to reduce the channel dimension from C to d

- collapse spatial dimensions (the encoder expects a sequence) and obtain d x HW feature map

- standard encoder architecture: MHA, FFN

- add positional encodings as the encoder is permutation invariant

<!-- image -->

88

# DETR

<!-- image -->

- as opposed to the standard architecture of the Transformer decoder, which operates in an auto-regressive manner, in DETR this module decodes the N objects in parallel

- object queries: learned N input embeddings (they are added to the input of each attention layer)

- the N object queries are transformed into an output embedding by the decoder

89

<!-- image -->

# DETR

Using self-attention and encoder-decoder attention over these embeddings, the model globally reasons about all objects together using pair-wise relations between them, while being able to use the whole image as context

<!-- image -->

Original transformer architecture

90

# DETR Object queries

<!-- image -->

91

# DETR

<!-- image -->

- 3-layer perceptron with ReLU and hidden dimension d and a linear projection layer

- predicts normalized center coordinates, height and width w.r.t. to the input image and the class label

- since N is usually much larger than the actual number of objects of interest in an image a special class label ∅ (no object/”background”) 

92

# DETR

<!-- image -->

<!-- image -->

93

# DETR

<!-- image -->

<!-- image -->

94

# DETR

- Second step is to compute the loss function for all the pairs previously matched:
- 
- 
- 
- 
- Bounding box loss: the box are predicted directly and not w.r.t. initial guesses

<!-- image -->

<!-- image -->

95

# DETR

<!-- image -->

96