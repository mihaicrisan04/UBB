# Multiple Choice — Test 03 (Basics, easier)

Same coverage as `mc-01.md`, but the questions stay close to **definitions and core ideas** — no niche details, no architecture-specific tricks. 30 questions across all the teacher's topic banks. Pick one (A/B/C/D) and write it on the `Answer:` line.

The five you missed last time are recycled here at the same difficulty so you can lock them in:
- Sigmoid range
- Dropout's purpose
- VGG's signature
- MSE vs cross-entropy
- mAP definition

---

## Basic layers in DL

**1. Which activation function is bounded in the range (0, 1)?**
- A) ReLU
- B) Tanh
- C) Sigmoid
- D) Linear

Answer: ______

**2. The Tanh activation function outputs values in the range:**
- A) (0, 1)
- B) [0, +∞)
- C) (−1, 1)
- D) (−∞, +∞)

Answer: ______

**3. The main role of an activation function in a neural network is to:**
- A) Reduce the number of parameters
- B) Introduce non-linearity so stacking layers produces more than just a single linear function
- C) Speed up matrix multiplication
- D) Replace the loss function

Answer: ______

**4. Dropout is a regularization technique that during training:**
- A) Randomly turns off (zeroes) some neurons so the network can't rely on any single one
- B) Speeds up the forward pass
- C) Replaces the optimizer
- D) Increases the learning rate dynamically

Answer: ______

**5. Batch normalization is mainly used to:**
- A) Replace pooling layers
- B) Eliminate the need for activations
- C) Stabilize and speed up training by normalizing layer inputs
- D) Reduce the total parameter count

Answer: ______

---

## CNN architectures

**6. A pooling layer (e.g. max pooling) has:**
- A) Many learnable parameters
- B) No learnable parameters
- C) The same number of parameters as a convolution
- D) Only a bias term

Answer: ______

**7. VGG networks are best known for:**
- A) Stacking many small 3×3 convolutions in series
- B) Residual (skip) connections
- C) Inception modules
- D) Depthwise separable convolutions

Answer: ______

**8. ResNet's most important contribution is:**
- A) Replacing pooling with attention
- B) Skip connections that let very deep networks train without vanishing gradients
- C) Depthwise separable convolutions
- D) Inception modules

Answer: ______

**9. AlexNet (2012) is historically important because it:**
- A) Was the first transformer-based model
- B) Was the first deep CNN to dominate ImageNet, popularizing ReLU, dropout, and GPU training
- C) Introduced residual connections
- D) Removed convolutions entirely

Answer: ______

---

## Optimization

**10. Gradient descent updates the weights by:**
- A) Stepping in the direction of the gradient
- B) Stepping in the *opposite* direction of the gradient, scaled by the learning rate
- C) Choosing weights at random
- D) Solving a system of linear equations

Answer: ______

**11. Compared to plain SGD, Adam:**
- A) Has no learning rate at all
- B) Adapts the learning rate per parameter using running averages of the gradient
- C) Is always less accurate
- D) Cannot be used on deep networks

Answer: ______

**12. The "learning rate" controls:**
- A) The size of each weight-update step
- B) The number of training epochs
- C) The batch size
- D) The number of layers in the network

Answer: ______

**13. Momentum in SGD helps to:**
- A) Reduce memory usage
- B) Speed up movement in consistent gradient directions and dampen oscillations
- C) Eliminate overfitting
- D) Choose the loss function

Answer: ______

---

## Loss functions

**14. Cross-entropy loss is the standard loss for:**
- A) Regression
- B) Classification
- C) Clustering
- D) Dimensionality reduction

Answer: ______

**15. Mean Squared Error (MSE) is the standard loss for:**
- A) Multi-class classification
- B) Regression tasks (predicting a continuous value)
- C) Clustering
- D) Object detection bounding-box classes

Answer: ______

**16. Dice loss is especially useful in semantic segmentation when:**
- A) The dataset is perfectly balanced
- B) Classes are heavily imbalanced (e.g. small foreground on huge background)
- C) The model is fully untrained
- D) There is only one class

Answer: ______

**17. Focal loss is designed to:**
- A) Replace gradient descent
- B) Focus the gradient on hard examples by down-weighting easy ones
- C) Add L2 regularization
- D) Speed up the softmax

Answer: ______

---

## Evaluation metrics

**18. Accuracy can be a misleading metric when:**
- A) The classes are perfectly balanced
- B) The classes are heavily imbalanced (e.g. 90% / 10%)
- C) You have many GPUs
- D) The model is shallow

Answer: ______

**19. Recall ("how many of the actual positives did we catch?") is defined as:**
- A) TP / (TP + FN)
- B) TP / (TP + FP)
- C) (TP + TN) / total
- D) FP / (FP + TN)

Answer: ______

**20. Intersection over Union (IoU) for a single class measures:**
- A) The pixel accuracy
- B) The training loss
- C) The ratio between the intersection and the union of the predicted region and the ground-truth region
- D) The number of detected classes per image

Answer: ______

**21. Mean Average Precision (mAP) is computed by:**
- A) Averaging accuracy across classes
- B) Computing Average Precision (area under the precision-recall curve) per class, then averaging across classes
- C) Counting bounding boxes per image
- D) Averaging IoU across all images

Answer: ______

---

## Transformers

**22. The three learned projections used in self-attention are called:**
- A) Encoder, decoder, residual
- B) Input, hidden, output
- C) Query, key, value
- D) Filter, bias, activation

Answer: ______

**23. Positional encodings are added in transformers because:**
- A) They speed up softmax
- B) Self-attention treats tokens as a *set* with no notion of order, so the model needs to be told where each token came from
- C) They reduce the parameter count
- D) They replace batch normalization

Answer: ______

**24. A Vision Transformer (ViT) processes an image by first:**
- A) Applying many convolutional layers
- B) Splitting the image into small patches, embedding each, and processing them as a sequence
- C) Running it through an LSTM
- D) Using fully connected layers on raw pixels

Answer: ______

**25. The special "CLS" token in a ViT is used to:**
- A) Mark the end of the sequence
- B) Aggregate global information from all patch tokens for use in the final classification
- C) Replace positional encoding
- D) Compute the loss directly

Answer: ______

---

## Segmentation

**26. Semantic segmentation differs from image classification in that it predicts:**
- A) A single label for the whole image
- B) A label for every pixel in the image
- C) Bounding boxes around objects
- D) Only the largest object

Answer: ______

**27. U-Net's skip connections help by:**
- A) Reducing overfitting
- B) Passing high-resolution feature maps from the encoder to the decoder to preserve spatial detail
- C) Replacing pooling
- D) Implementing attention

Answer: ______

---

## Visualization

**28. Grad-CAM produces:**
- A) A 3D rendering of the network
- B) A class-specific heatmap showing which regions of the input were most important for the prediction
- C) The training loss curve
- D) A confusion matrix

Answer: ______

---

## Object detection

**29. Non-Maximum Suppression (NMS) is used to:**
- A) Train the detector
- B) Generate anchor boxes
- C) Remove duplicate overlapping bounding boxes for the same object, keeping the highest-scoring one
- D) Compute the loss

Answer: ______

---

## SSL / Multimodal

**30. CLIP is trained to:**
- A) Generate images from text only
- B) Align image and text representations in a shared embedding space using a contrastive objective over (image, caption) pairs
- C) Detect objects in images
- D) Translate between languages

Answer: ______

---

When done, ping me and I'll grade in-file.
