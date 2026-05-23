# Computer vision and deep learning - lecture 2


## Page 1

Computer vision and deep 
learning
Lecture 2

## Page 2

Today’s agenda
• Linear classifiers
• Evaluating a classifier
• Loss functions
• Gradient descent (to be continued)

## Page 3

Image classification I
Classifier CAT*
Image source: https://katzenworld.co.uk/2019/06/30/mews-funny-cat-picture-collection/
* assuming that we 
have a set of labels L = 
{cat, shark, ship …}
Xi - image sample to be classifier
yi- ground truth
𝑦ෝ𝑖- prediction

## Page 4

Image classification II
• Unlike other computer science problems:
• Find the maximum value in an array
• Sort an array
• Find the shortest distance between two nodes in a graph
• No obvious way to implement an algorithm for image classification

## Page 5

Classification challenges
View point variations
Occlusions
Scale variation
Deformation
Lightning conditions
Background clutter
Intra-class variation

## Page 6

Linear classification

## Page 7

Linear classification
• Define a function which maps image pixels to class scores
𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏
Classifier 
𝑓 𝑥𝑖, 𝑊, 𝑏
N class scores
weights bias

## Page 8

Image representation
w
h
red
green
blue
w
h
……
1D vector with w*h*3 values between [0, 255]
Flatten the 
image to a 1D 
array

## Page 9

Linear classification
CIFAR 10 example: 
• 3 channel images, 32x32 pixels
• N = 10 class scores
, 𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 ⋅ 𝑥𝑖 + 𝑏
Classifier 
𝑓 𝑥𝑖, 𝑊, 𝑏
N class scores
D = 32x32x3 = 3072
N = 10 
xi ∈ ℝD
f: ℝD → ℝN

## Page 10

Linear classification
CIFAR 10 example: 
• 3 channel images, 32x32 pixels, xi size [3072 x 1] 
• N = 10 class scores
𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏
Classifier 
𝑓 𝑥𝑖, 𝑊, 𝑏
N class scores
D = 32x32x3 = 3072
N = 10 
xi ∈ ℝD
f: ℝD → ℝN
[3072 x 1]
??? [D x 1]

## Page 11

Linear classification
CIFAR 10 example: 
• 3 channel images, 32x32 pixels
• N = 10 class scores
𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏
Classifier 
𝑓 𝑥𝑖, 𝑊, 𝑏
N class scores
D = 32x32x3 = 3072
N = 10 
xi ∈ ℝD 
f: ℝD → ℝN
size: N x D 
10 x 3072
[D x 1]
[3072 x 1]

## Page 12

Linear classification
CIFAR 10 example: 
• 3 channel images, 32x32 pixels
• N = 10 class scores
𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏
Classifier 
𝑓 𝑥𝑖, 𝑊, 𝑏
N class scores
D = 32x32x3 = 3072
N = 10 
xi ∈ ℝD 
f: ℝD → ℝN
size: N x D 
10 x 3072
size: N x 1 
10 x 1 
[D x 1]
[3072 x 1]

## Page 13

Linear classification - simplified example
57 77
200 128
57
77
200
128
0.7 -0.3 2.5 1
-0.3 0.5 3.1 -3
0 0.25 0.4 0.1
Flattened input image
weights W 
1.7
-2
3
+ = 
646.5
255.4
115.05
bias b 
X
cat score
boat score
shark score
Example adapted from: https://cs231n.github.io/
·

## Page 14

Linear classification – implementation hints
The bias trick
57 77
200 128
57
77
200
128
0.7 -0.3 2.5 1
-0.3 0.5 3.1 -3
0 0.25 0.4 0.1
Flattened input image
weights W 
1.7
-2
3
+ = 
646.5
255.4
115.05
bias b 
X
cat score
boat score
shark score
Example adapted from: https://cs231n.github.io/
·

## Page 15

Linear classification – implementation hints
The bias trick
57 77
200 128
57
77
200
128
1
0.7 -0.3 2.5 1
-0.3 0.5 3.1 -3
0 0.25 0.4 0.1
Flattened input image
weights W 
= 
646.5
255.4
115.05
bias b 
X
cat score
boat score
shark score
Example adapted from: https://cs231n.github.io/
·
1.7
-2
3

## Page 16

Linear classification - simplified example
57 77
200 128
3
Example adapted from: https://cs231n.github.io/
0.7 -0.3
2.5 1
-0.3 0.5
3.1 -3
0 57 0.25
0.4 0.14
1.7 -2
0.7 -0.3 2.5 1
-0.3 0.5 3.1 -3
0 0.25 0.4 0.1
W
b
score 646.5 255.4 115.05

## Page 17

Linear classification – visual interpretation
Image source: https://cs231n.github.io/
• Linear classification can be seen as a template matching process, 
where the weights are learned
• Each row of the weights matrix is a prototype (template) for one 
of the classes

## Page 18

Linear classification – geometric interpretation
Image source: https://cs231n.github.io/
Hyper-plane cutting up the space
, 𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏

## Page 19

Linear classification
• Input xi → fixed
• N: number of classes →fixed
• W – weights → we have control over their setting
• b – bias → we have control over their setting
• b doesn’t actually interact with the data
• f – score function - map raw pixels to class scores
Goal: set W and b such that the correct class has a higher value than 
in incorrect ones
𝑓 𝑥𝑖, 𝑊, 𝑏 = 𝑊 𝑥𝑖 + 𝑏

## Page 20

Next steps
• Define a loss function L
• L - measures the quality of W and b based on how well the predicted scores 
agree with the ground truth labels
• L – how happy are we with the scores across the training data? How good is 
our (current) classifier?
• Find an efficient way to determine the parameters (W and b) such 
that the loss function is minimized

## Page 21

Loss function
• N – number of training examples
• 𝑥𝑖- i
th image in our training dataset
• 𝑦𝑖- ground truth label for the i
th image in our training set (i.e. cat, 
boat or shark) 
𝐿 =
1
𝑁
෍
𝑖
𝑁
𝐿𝑖 (𝑓 𝑥𝑖, 𝑊 , 𝑦𝑖)

## Page 22

Example – softmax classifier
Class
cat 3.2
boat -5
shark 10
We don’t have interpretation for the outputs of the 
linear layer
What if we would like to determine the probability of 
each class?

## Page 23

Example – softmax classifier
Class
cat 3.2
boat -5
shark 10
We don’t have interpretation for the outputs of the 
linear layer
What if we would like to determine the probability of 
each class?
𝑃 𝑌 = 𝑐 𝑋 = 𝑥𝑖 ) =
𝑒
𝑠𝑐
σ𝑗
𝑒
𝑠𝑗
S = 𝑓(𝑥𝑖, 𝑊)
Softmax function

## Page 24

Example – softmax classifier
Class
cat 3.2
boat -5
shark 10
So far, we don’t have interpretation for the outputs of 
the score function
What if we would like to determine the probability of 
each class?
𝑃 𝑌 = 𝑐 𝑋 = 𝑥𝑖 ) =
𝑒
𝑠𝑐
σ𝑗
𝑒
𝑠𝑗
S = 𝑓(𝑥𝑖, 𝑊)
Softmax function 
normalize

## Page 25

Class
cat 3.2
boat -5
shark 10
s = 
[
3.2
-5
10
]
𝑒
𝑠 = 
[
24.5325
0.0067
22026.4658
]
σ𝑗 𝑒
𝑠𝑗 = 22026.4658 
𝑒
𝑠𝑐
σ𝑗
𝑒
𝑠𝑗
=
[
0.0011125
0.0000003
0.9988872
]
softmax function
[
0.0011125
0.0000003
0.9988872
]
softmax function
vs
hardmax function, one hot encoding
[
1
0
0
]
0 – cat class label, 1 – boat, 2 – shark class

## Page 26

𝐿𝑖 = −෍
𝑗=1
𝐶
𝑦𝑗log 𝑠𝑗
𝑠𝑗- prediction for class j
= − 𝑦𝑐log 𝑠𝑐
= − log(𝑠𝑐)
Cross entropy loss-function
log 𝑥
− log 𝑥

## Page 27

𝐿𝑖 = −෍
𝑗=1
𝐶
𝑦𝑗log 𝑠𝑗
𝑠𝑗- prediction for class j
= − 𝑦𝑐log 𝑠𝑐
= − log(𝑠𝑐)
Intuition:
• The loss 𝐿𝑖 = − log(𝑠𝑐) should be as small as possible
→ the correct probability class should be as big as 
possible
[
0.0011125
0.0000003
0.9988872
]
[
1
0
0
]

## Page 28

𝐿𝑖 = −෍
𝑗=1
𝐶
𝑦𝑗log 𝑠𝑗
𝑠𝑗- prediction for class j
= − 𝑦𝑐log 𝑠𝑐
= − log(𝑠𝑐)
Intuition:
• The loss 𝐿𝑖 = − log(𝑠𝑐) should be as small as possible
→ the correct probability class should be as big as 
possible
This loss function just looks at the correct class from the 
ground truth and tries to make the predicted probability 
of that class as big as possible

## Page 29

• What is the minimum and maximum value of 𝐿𝑖?
𝐿 =
1
𝑁
෍
𝑖
𝑁
𝐿𝑖
𝐿𝑖 = −log(𝑠𝑐)

## Page 30

• What is the minimum and maximum value of 𝐿𝑖?
• Min 0
• Max infinity
𝐿𝑖 = −log(𝑠𝑐)
𝐿 =
1
𝑁
෍
𝑖
𝑁
𝐿𝑖

## Page 31

Example – focal loss
• Problems with cross-entropy loss
• Class imbalance is a problem: majority class examples will dominate the loss 
function and gradient descent, causing the weights to update in the direction 
of the model becoming more confident in predicting the majority class while 
putting less emphasis on the minority classes
• Fails to distinguish between hard and easy examples.

## Page 32

Example – Balanced CE loss
• Balanced Cross-Entropy Loss (BlCE) – add a weighing factor to each 
class 𝛼𝑗
• could be the inverse class frequency or a hyper-parameter that is determined 
by cross-validation
• Solves class imbalance problem
𝐶𝐸 = −෍
𝑗=1
𝐶
𝑦𝑗log 𝑠𝑗 B𝑙𝐶𝐸 = −෍
𝑗=1
𝐶
𝑦𝑗⋅ 𝛼𝑗log 𝑠𝑗

## Page 33

Example – Focal loss
• Focuses on the examples that the model gets wrong rather than the 
ones that it can confidently predict
• Ensures predictions on hard examples improve over time rather than 
becoming overly confident with easy ones.
• Down weighting
• reduce the influence of easy examples on the loss function, resulting in more 
attention being paid to hard examples.

## Page 34

Example – focal loss
• Down weighting
• reduce the influence of easy examples on the loss function, resulting in more 
attention being paid to hard examples.
• 𝛾 – focusing factor; hyperparameter
• 1 − 𝑝𝑡
𝛾
- modulating factor
𝐶𝐸 = −෍
𝑗=1
𝐶
𝑦𝑗log 𝑠𝑗 𝐹𝐿 = −෍
𝑗=1
𝐶
𝑦𝑗⋅ 1 − 𝑠𝑗
𝛾
log 𝑠𝑗
𝐹𝐿 = −𝑝𝑡 1 − 𝑝𝑡
𝛾
log 𝑝𝑡
𝑝𝑡- correct class prediction

## Page 35

Example 
– focal loss
• Misclassification: 𝑝
𝑡
-?
𝐹𝐿=−1−𝑝
𝑡
𝛾
log𝑝
𝑡

## Page 36

Example 
– focal loss
• Misclassification: 𝑝
𝑡
- small → 
1−𝑝
𝑡
𝛾
≈1 → behaves like 
CE
• Correct and confident 
prediction ?
𝐹𝐿=−1−𝑝
𝑡
𝛾
log𝑝
𝑡

## Page 37

Example 
– focal loss
• Misclassification: 𝑝
𝑡
- small → 
1−𝑝
𝑡
𝛾
≈1 → behaves like 
CE
• Correct and confident 
prediction: 𝑝
𝑡
≈ 1 → 
1−𝑝
𝑡
𝛾
≈0 → down-
weighting the loss value for 
well-classified examples
𝐹𝐿=−1−𝑝
𝑡
𝛾
log𝑝
𝑡

## Page 38

Example 
– focal loss
•𝛾≥1 rescales the modulating 
factor such that the easy 
examples are down-weighted 
more than the hard ones
• e.g. 𝛾=2
•𝑝
1
=0.6 : CE: 0.2218, FL = 
0.035488; 6.25 decrease
•𝑝
2
=0.9 : CE = 0.04575, FL = 
0.0004575; 100x decrease
𝐹𝐿=−1−𝑝
𝑡
𝛾
log𝑝
𝑡

## Page 39

Example – focal loss
• In practice, focal loss is combined it balanced cross entropy loss by 
adding also the weighing factor 𝛼𝑡
𝐹𝐿 = − 𝛼𝑡 1 − 𝑝𝑡
𝛾
log 𝑝𝑡

## Page 40

Example – hinge loss or SVM loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)

## Page 41

Example – hinge loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿 𝑖= ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
if 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
 return 0
else:
 return 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚

## Page 42

Example – hinge loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿 𝑖= ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
if 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
 return 0
else:
 return 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚
= ෍
𝑗 !=𝑦𝑖
൝
0, 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
𝑚 + 𝑠𝑗 − 𝑠𝑦𝑖, 𝑜𝑡ℎ𝑒𝑟𝑤𝑖𝑠𝑒

## Page 43

Example – hinge loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿 𝑖= ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
if 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
 return 0
else:
 return 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚
= ෍
𝑗 !=𝑦𝑖
൝
0, 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
𝑚 + 𝑠𝑗 − 𝑠𝑦𝑖, 𝑜𝑡ℎ𝑒𝑟𝑤𝑖𝑠𝑒
Intuition: we are “happy” if the score of the correct class if larger, by a margin of at least 
m, than the score of the other classes

## Page 44

Example – hinge loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿 𝑖= ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
= ෍
𝑗 !=𝑦𝑖
൝
0, 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
𝑚 + 𝑠𝑗 − 𝑠𝑦𝑖, 𝑜𝑡ℎ𝑒𝑟𝑤𝑖𝑠𝑒
𝑠𝑦𝑖𝑠𝑗
m
loss

## Page 45

Example – hinge loss
• 𝑠𝑗 = 𝑓 𝑥𝑗, 𝑊 - the prediction of the j
th image in our training dataset
• m – margin 
𝐿 𝑖= ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
= ෍
𝑗 !=𝑦𝑖
൝
0, 𝑠𝑦𝑖 ≥ 𝑠𝑗 + 𝑚
𝑠𝑗 − 𝑠𝑦𝑖, 𝑜𝑡ℎ𝑒𝑟𝑤𝑖𝑠𝑒
m
𝑠𝑦𝑖𝑠𝑗
loss

## Page 46

Example – hinge loss
𝐿𝐻𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
Let’s set m = 1
Class
cat 3.2 0.1 0.3
boat -5 14 8
shark 10 5 14
Hinge 
loss
For image 1
=max(0, - 5 – 3.2 + 1) + max(0, 10 – 3.2 + 1)
= 0 + 7.8
= 7.8

## Page 47

Example – hinge loss
𝐿𝐻𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
Let’s set m = 1
For image 1
=max(0, - 5 – 3.2 + 1) + max(0, 10 – 3.2 + 1)
= 0 + 7.8
= 7.8
Class
cat 3.2 0.1 0.3
boat -5 14 8
shark 10 5 14
Hinge 
loss
7.8

## Page 48

Example – hinge loss
𝐿𝐻𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)
Let’s set m = 1
For image 2
= max(0, 0.1 – 14 + 1) + max(0, 5 – 14 + 1)
= 0 + 0
= 0
For image 3
= max(0, 0.3 – 14 + 1 ) + max(0, 8 – 14 + 1)
= 0 + 0
= 0
Class
cat 3.2 0.1 0.3
boat -5 14 8
shark 10 5 14
Hinge 
loss
7.8 0 0
Total loss L 
𝐿 =
1
𝑁
෍
𝑖=1
𝑁
𝐿𝑖
L = (7.8 + 0 + 0 )/3 = 2.6

## Page 49

Hinge loss
• What is the min value and the max 
value of hinge loss?
𝑠𝑦𝑖𝑠𝑗
loss
𝐿 𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 1)

## Page 50

Hinge loss
• What is the min value and the max 
value of hinge loss?
• min is 0: the correct score is much 
larger (>=m ) than the incorrect scores
• max is infinity 
• In the beginning of the training (W ≈ 
0) so all scores ≈ 0. What is the 
value of L ?
𝑠𝑦𝑖𝑠𝑗
loss
𝐿𝐻𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 1)

## Page 51

Hinge loss
• What is the min value and the max 
value of hinge loss?
• In the beginning of the training (W ≈ 
0) so all scores ≈ 0. What is the 
value of L ?
• C – 1
𝑠𝑦𝑖𝑠𝑗
loss
𝐿𝐻𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 1)

## Page 52

Regularization
• Suppose that you found some values for the parameters W such that 
the loss function is 0. Do you think there is a single setting for the 
parameters W, such that the loss is 0?

## Page 53

Regularization
• Suppose that you found some values for the parameters W such that 
the loss function is 0. Do you think there is a single setting for the 
parameters W, such that the loss is 0?
• W, 2W, 3W, αW
• extend the loss function with a regularization penalty to 
discourage large weights

## Page 54

Regularization
Image source: https://ro.pinterest.com/pin/192669690286459858/?nic_v2=1a57hjLPe
Occam’s razor: "the simplest explanation is most 
likely the right one"
𝐿 =
1
𝑁
෍
𝑖=1
𝑁
𝐿𝑖 + λ𝑅(𝑊)
Data loss
Regularization loss
• Prevent overfitting (the model will perform 
”too” well on the training data)
• Occam’s razor: William of Ockham (c. 1287–
1347)
• Make the model simpler

## Page 55

Regularization
• L2 regularization
• 𝑅 𝑊 = σ𝑟 σ𝑐 𝑊𝑟,𝑐
2
x = [2, 0.1, 0.1, 15]
W1 = [0.2, 0.8, 0.2, 0.1] → R(W) = 0.73
W2 = [1, 0, 0, 0] → R(W) = 1 
W1· X = 2
W2· X = 2

## Page 56

Regularization
• L2 regularization
• 𝑅 𝑊 = σ𝑟 σ𝑐 𝑊𝑟,𝑐
2
x = [2, 0.1, 0.1, 15]
W1 = [0.2, 0.8, 0.2, 0.1] → R(W) = 0.73
W2 = [1, 0, 0, 0] → R(W) = 1
W1· X = 2
W2· X = 2
Prefers to “spread out” the weights such that each element has the same influence on 
the prediction

## Page 57

Regularization
• L1 regularization
• 𝑅 𝑊 = σ𝑟 σ𝑐| 𝑊𝑟,𝑐|
x = [2, 0.1, 0.1, 15]
W1 = [0.2, 0.8, 0.2, 0.1] → R(W) = 1.3
W2 = [1, 0, 0, 0] → R(W) = 1 
W1· X = 2
W2· X = 2

## Page 58

Regularization
• L1 regularization
• 𝑅 𝑊 = σ𝑟 σ𝑐| 𝑊𝑟,𝑐|
x = [2, 0.1, 0.1, 15]
W1 = [0.2, 0.8, 0.2, 0.1] → R(W) = 1.3
W2 = [1, 0, 0, 0] → R(W) = 1 
W1· X = 2
W2· X = 2
• Prefers more sparse weight matrices, most of the values in W should 
be close to 0, except for some values where the weights are allowed to 
deviate
• Feature selection

## Page 59

Evaluating a classifier

## Page 60

Evaluating a classifier
"Facts are stubborn things, but statistics are more pliable." – Mark Twain
"There are three kinds of lies: lies, damned lies, and statistics." – popularized by 
Mark Twain

## Page 61

Train, dev and test sets
• Training set
• Used to train the model, determines what the network learns
• Development (dev) set or validation set
• Used to tune, iterate, and make decisions: e.g. used to evaluate model 
performance to guide decisions on hyperparameter settings, compare 
different models or iterations and select the one that performs best.
• Test set
• Used to get an unbiased estimate of the final performance of the model
Sometimes is might be ok to not have a test set (only train and dev
sets)

## Page 62

How to split your data into train/dev/test set?
• Before deep learning, when available data was relatively limited (e.g. 10000 
images)
70 % train 30 % dev
60 % train 20% dev 20% dev
7000 train, 3000 dev
6000 train, 2000 dev,
2000 test

## Page 63

K-fold validation

## Page 64

Stratified k-fold validation

## Page 65

How to split your data into train/dev/test set?
• Large scale datasets
• ImageNet > 14 million images 
• VGGFace 2 > 3.3 million images
• E.g. 1 million images
• 980000 train set, 10000 dev set, 10000 test set -> 98% train, 1% dev, 1% test
• E.g. 4 million images
• 3980000 train set, 10000 dev set, 10000 test set -> 99.5% train, 0.25% dev, 
0.25% test

## Page 66

Confusion Terms
TP
True Positive
Predicted this class: actually this class
FP
False Positive
Predicted this class: actually something else
FN
False Negative
Did NOT predict this class: but it actually was
TN
True Negative
Did NOT predict: and it wasn't this class

## Page 67

Accuracy
Accuracy = Correct Predictions / Total Predictions 
= (TP + TN) / (TP + TN + FP + FN)
Beware class imbalance — If 90% of samples are class A, a model predicting A every time gets 90% accuracy — but is 
useless. Always pair with per-class metrics. !!!Problem!!! If 90% of samples are class A, a model predicting A every time gets 90% accuracy, but is useless.

## Page 68

Binary Precision & Recall
PrecisiON
TP / (TP + FP)
The ability to identify ONly relevant samples
When the model says it’s positive, how often is it right?
RecALL
TP / (TP + FN)
The ability to identify ALL relevant samples
Out of all the actual positives, how many did the model find?
F1 Score = 2 × (Precision × Recall) / (Precision + Recall)
Harmonic mean of precision and recall. It combines both in a single metric

## Page 69

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90

## Page 70

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90
Accuracy = (TP + TN)/(TP + TN + FN + FP) = (1 + 90)/(1 + 90 + 8 + 1) = 91/100 = 0.91
Precision = TP / (TP + FP) = 1 / (1 + 1) = 0.5
Recall = TP / (TP + FN) = 1 / (1 + 8) = 0.11

## Page 71

Confusion matrix
Image source: https://en.wikipedia.org/wiki/Confusion_matrix

## Page 72

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90
RECALL family: Of the real positives, how did the classifier do?
Recall or Sensitivity or TPR (0.11): "Of all truly malignant patients, what fraction did I catch?" Used in medical screening
FNR or Miss Rate (0.89):"Of all truly malignant patients, what fraction did I miss?" FNR = 1 − Recall.

## Page 73

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90
SPECIFICITY family: "Of the real negatives, how did the classifier do?“
Specificity or TNR (= TN / (TN + FP) = 90/91 = 0.989): "Of all truly benign patients, what fraction did I correctly clear?“ 
Many false alarms?
FPR or Fall-out (0.011):"Of all truly benign patients, what fraction did I wrongly flag as malignant?“. FPR = 1 − Specificity.

## Page 74

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90
PRECISION family: "When the classifier makes a positive prediction, how trustworthy is it? “
Precision or PPV (0.50): "Of every patient I flagged as malignant, how many actually were?“
FDR (0.50): "Of every patient I flagged as malignant, how many were actually benign?“. FDR = 1 − Precision.

## Page 75

Binary Precision & Recall – tumor classification example
True positive
Reality: malignant
Model predicted: malignant
1
False positive
Reality: benign
Model predicted: malignant
1
False negative
Reality: malignant
Model predicted: benign
8
True negative
Reality: benign
Model predicted: benign
90
NPV family: "When the classifier makes a negative prediction, how trustworthy is it?
NPV (=90/98=0.918): "Of every patient I cleared as benign, how many actually were benign?“ Are negative calls reliable?
FOR (0.082): "Of every patient I cleared as benign, how many were actually malignant?“.
These are the dangerous silent misses. FOR = 1 − NPV.

## Page 76

Precision vs. recall
Precision: 0.8
Recall: 0.73
Precision: 0.88
Recall: 0.64
Precision: 0.75
Recall: 0.82
https://developers.google.com/machine-learning/crash-course/classification/precision-and-recall

## Page 77

• Precision: spam detection, predict when to launch a satellite, 
pregnancy tests etc.
• Recall: airport security (make sure that every suspicions event is 
investigated), cancer prediction, detecting credit card frauds etc.
Precision vs. recall – when to favour one vs the 
other?

## Page 78

Receiver Operating characteristic curve
ROC curve
• Shows the performance of a classifier at different classification 
thresholds
TPR
FPR
False Positive Rate (1- specificity) = FP / (FP + TN) – x axis
True Positive Rate (sensitivity) = TP / (TP + FN) – y axis

## Page 79

https://towardsdatascience.com/roc-curve-and-auc-from-scratch-in-numpy-visualized-2612bb9459ab
Receiver Operating characteristic curve
ROC curve

## Page 80

https://towardsdatascience.com/roc-curve-and-auc-from-scratch-in-numpy-visualized-2612bb9459ab
Receiver Operating characteristic curve
ROC curve

## Page 81

https://towardsdatascience.com/roc-curve-and-auc-from-scratch-in-numpy-visualized-2612bb9459ab
Receiver Operating characteristic curve
ROC curve

## Page 82

• Shows the performance of a classifier at different classification 
thresholds
Receiver Operating characteristic curve
ROC curve
False Positive Rate (1- specificity) = FP / (FP + TN) – x axis
True Positive Rate (sensitivity) = TP / (TP + FN) – y axis

## Page 83

• Measures the entire two-dimensional area underneath the entire 
ROC curve
• Makes it easier to compare one ROC curve to another one
Receiver Operating characteristic curve
Area under the curve

## Page 84

• In multi-class settings, we don't just have positive and negative.
• We evaluate each class individually as if it were a binary problem.
Multi-class settings
Macro average Micro average
Calculate metric per class, then average them. Aggregate all TP, FP, FN first, then calculate.
Treats every class as equally important. Treats every sample as equally important.
Useful for imbalanced data where the small 
class is important. General performance when class size varies.

## Page 85

Multi-class settings
Example
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/
TP? FP? FN?

## Page 86

Multi-class settings
Example
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/
Airplane: TP = 2, FP = 1, FN = 1
Boat: TP = 1, FP = 3, FN = 0
Car: TP = 3, FP = 0, FN = 3

## Page 87

Multi-class settings
Example
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/
Airplane: TP = 2, FP = 1, FN = 1
Precision = 2/3 ~ 0.67
Recall = 2/3 ~ 0.67
Boat: TP = 1, FP = 1, FN = 0
Precision = 1/4 = 0.25
Recall = 1/1 = 1

## Page 88

Multi-class settings
Macro average
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/
Calculate metric per class, then average them.
Precision = (0.67 + 0.25 + 1)/3 = 1.92/3 = 0.64
Recall = (0.67 + 1 + 0.5)/3 = 0.72
F1 = (0.67 + 0.4 + 0.67)/3 = 1.74/3 = 0.58

## Page 89

Multi-class settings
Weighted macro average
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/

## Page 90

Multi-class settings
Micro average
https://iamirmasoud.com/2022/06/19/understanding-micro-macro-and-weighted-averages-for-scikit-learn-metrics-in-multi-class-classification-with-example/
We first sum the respective TP, FP, and FN values across all classes and then plug 
them into the equations to get the score.
The proportion of correctly classified observations out of all observations
Precision = TP / (TP + FP) = 6 / (6 + 4) = 0.6
Recall = TP/(TP + FN) = 6 / (6 + 4) = 0.6
F1 = 2*(Precision * Recall) / (Precision + Recall) = 2 * 0.36/1.2 = 0.6

## Page 91

Optimization
How can we find the best value for W?

## Page 92

Follow the slope!
Gradient descent

## Page 93

Follow the slope
• In 1D the derivative of a function is:
• Slope of the “line”
𝑓
′ 𝑎 =
𝜕𝑓(𝑥)
𝜕𝑥
= lim
ℎ →0
𝑓 𝑎 + ℎ − 𝑓(𝑎)
ℎ

## Page 94

Follow the slope
Image source: https://medium.com/analytics-vidhya/stochastic-gradient-descent-1ab661fabf89

## Page 95

Follow the slope
• In N dimensions, the gradient is a vector containing the partial 
derivative along each dimensions
Numerical gradient
[0.3, 0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
gradient ?

## Page 96

Numerical gradient
[0.3,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
W + h
h = 0.0001
gradient ?
[0.3001,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2331
= 0.3 + h
= 0.3 + 0.0001 𝑓
′ 𝑎 =
𝜕𝑓(𝑥)
𝜕𝑥
= lim
ℎ →0
𝑓 𝑎 + ℎ − 𝑓(𝑎)
ℎ

## Page 97

Numerical gradient
[0.3,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
W + h
h = 0.0001
gradient [(1.2345 - 1.2331)/0.0001, ….]
[0.3001,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2331
= 0.3 + h
= 0.3 + 0.0001

## Page 98

Numerical gradient
[0.3,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
W + h
h = 0.0001
gradient [23.5, ….]
[0.3001,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2331
= 0.3 + h
= 0.3 + 0.0001

## Page 99

Numerical gradient
[0.3,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
W + h
h = 0.0001
gradient [23.5, … ]
[0.3, 0.2401, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
= 0.24 + h

## Page 100

Numerical gradient
[0.3,0.24, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
W
W + h
h = 0.0001
gradient [23.5, (1.2345 – 1.2345)/0.0001, ….]
[0.3001,0.2401, -1.9, 0.22, 0.3, 0.9, -1.7, 1.5, 0.29,…]
loss 1.2345
= 0.24 + h

## Page 101

Analytical gradient
• Numerical gradient: slow and approximate
• Analytical gradient. Given:
, we need to compute 
𝜕𝐿
𝜕𝑊
=
1
𝑁
෍
1
𝑁
𝜕𝐿𝑖(𝑥𝑖, 𝑦𝑖, 𝑊)
𝜕𝑊 + λ
𝜕𝑅(𝑊)
𝜕𝑊
and we can use calculus for this!
𝐿 =
1
𝑁
෍
𝑖=1
𝑁
𝐿𝑖 (𝑥𝑖, 𝑦𝑖, 𝑊) + λ𝑅(𝑊) 𝐿𝑖 = ෍
𝑗 !=𝑦𝑖
max(0, 𝑠𝑗 − 𝑠𝑦𝑖 + 𝑚)

## Page 102

Gradient descent
while True:
 W_gradient = compute_gradient(loss_func, data, W)
 # update parameters
 W += -lr*W_gradient
Example adapted from: https://cs231n.github.io/

## Page 103

Recommended resources
• https://cs231n.github.io/linear-classify/#intro
• Loss functions (Andrew Ng`s great explanations):
• https://www.youtube.com/watch?v=LLux1SW--oM
• https://www.youtube.com/watch?v=ueO_Ph0Pyqk
• www.coursera.org – Deep Learning specialization: Neural Networks 
and Deep Learning (week 1 and 2)