# CVDL — Lecture Cheatsheet

The whole course, distilled. Organized by the teacher's exam topic banks. Plain English. Math skipped.

---

## 1. Basic layers in DL

**Linear layer**: `f(x) = Wx + b`. The fundamental building block.

**Activations** (needed — otherwise stacked linears collapse into one linear):
- **ReLU**: `max(0, x)`. Default in CNNs. Risk: "dying ReLU" (stuck at 0).
- **Leaky ReLU**: small slope for negatives. Fixes dying ReLU.
- **Sigmoid**: squashes to **(0, 1)**. Used in binary final layers, attention gates. Vanishing gradients in deep nets.
- **Tanh**: squashes to **(−1, 1)**. Zero-centered. Used in RNNs/LSTMs.
- **GELU**: smooth, probabilistic. Default in transformers (ViT).
- **Swish** (`x · sigmoid(x)`): used in EfficientNet, MobileNetV3.

**Batch normalization**: normalize pre-activations to zero mean & unit variance **across the batch**, then learn scale/shift. Faster training, less init sensitivity.

**Layer normalization**: same idea but across the **feature dimension** per sample. Independent of batch size → used in transformers.

**Dropout**: at training, randomly zero some activations. Forces redundancy → less overfitting. Off at test time, activations rescaled.

---

## 2. CNN architectures

| Year | Network | One-line summary |
|---|---|---|
| 1998 | LeNet | First CNN (digits) |
| 2012 | **AlexNet** | First deep CNN to win ImageNet. ReLU + dropout + GPUs |
| 2014 | **VGG** | Only 3×3 convs + 2×2 max-pool. Two 3×3 = one 5×5 receptive field, fewer params |
| 2014 | **GoogLeNet / Inception** | Parallel 1×1, 3×3, 5×5 conv + pool, concat. 1×1 used as bottleneck. No FC layers (uses GAP) |
| 2015 | **ResNet** | Skip connections (`output = F(x) + x`). Trains 100+ layers. Solves vanishing gradients |
| 2018 | **SENet** | Channel attention: squeeze (GAP) + excitation (2 FC + sigmoid) → per-channel weights |
| 2017 | MobileNet | **Depthwise separable conv** = depthwise (per channel) + 1×1 pointwise. Cheap |
| 2017 | ShuffleNet | Group convs + channel shuffle (so groups exchange info) |
| 2019 | **EfficientNet** | **Compound scaling**: balance depth, width, resolution with one coefficient |

**Key conv terms**:
- **Stride** = step the filter moves.
- **Padding** = zeros around input so filter reaches edges.
- **1×1 convolution** = mixes channels, no spatial extent. Used as bottleneck to reduce depth.
- **Global Average Pooling (GAP)** = average each feature map to one number. Replaces FC layers at the end.
- **Pooling has NO learnable parameters.**

---

## 3. Optimization

**Gradient descent**: `θ -= lr · ∇L`.

**Batch sizes**:
- SGD (bs=1): noisy, slow.
- Mini-batch (32-512): standard.
- Full batch: too slow per step.

**Optimizers**:
- **SGD + momentum**: exponential weighted average of gradients. Accelerates consistent directions, damps oscillations. "Ball rolling with friction."
- **Nesterov**: look ahead before computing gradient. Slightly better.
- **AdaGrad**: per-parameter LR via sum of squared gradients. Decays too aggressively.
- **RMSProp**: AdaGrad with EMA (forgets old gradients).
- **Adam**: RMSProp + momentum. **Default optimizer**.
- **AdamW**: Adam with weight decay **decoupled** from gradient. Default for transformers/LLMs.
- **Lion**: signed updates only, less memory.

**Learning rate schedules**:
- **Step decay**: drop by factor every N epochs.
- **Cosine annealing**: smooth cosine from η_max → η_min.
- **Linear warmup**: start tiny, ramp up. Almost always used with transformers.
- Common combo: **warmup + cosine decay**.

**Initialization** (random init matters in deep nets):
- Zero → all neurons identical (symmetry problem). Bad.
- **Xavier / Glorot**: `Var(W) = 1/fan_in`. For tanh/sigmoid.
- **He**: `Var(W) = 2/fan_in`. For ReLU.

**Regularization**:
- **L2 weight decay**: prefer small, spread-out weights.
- **L1**: prefer sparse weights (feature selection).
- **Dropout, batch norm, data augmentation** all regularize.

**Bias vs variance**:
- **High bias** (underfit) → train longer / bigger model.
- **High variance** (overfit) → more data / more regularization / different architecture.

**Data augmentation**:
- **Cutout**: erase a random rectangle. Label unchanged.
- **Mixup**: blend two images and their labels.
- **CutMix**: paste a patch from B onto A; labels mixed by area.

---

## 4. Evaluation metrics

**Classification (from confusion matrix: TP/FP/FN/TN)**:
- **Accuracy** = (TP+TN)/all. **Misleading under class imbalance.**
- **Precision** = TP/(TP+FP). "When model says positive, how often right?"
- **Recall** (sensitivity, TPR) = TP/(TP+FN). "Of all positives, how many caught?"
- **F1** = harmonic mean of P and R.
- **Specificity** (TNR) = TN/(TN+FP). **FPR** = 1 − specificity.
- **ROC curve** = TPR vs FPR. **AUC** = area under it.

**Multi-class averaging**:
- **Macro**: per-class metric → average. Treats classes equally.
- **Micro**: aggregate TP/FP/FN across all classes first → compute. Treats samples equally.
- **Weighted**: macro, weighted by class frequency.

**When to prefer**: precision → spam, satellites. Recall → cancer detection, fraud.

**Segmentation**:
- **Pixel accuracy** → misleading (background dominates).
- **IoU** = intersection/union (per class).
- **mIoU** = mean IoU across classes. **The standard metric.**
- **Frequency-weighted IoU**: classes weighted by pixel count.
- **Dice coefficient** = 2·|A∩B|/(|A|+|B|). Always ≥ IoU. Same rankings.
- **Boundary IoU**: only on contour. Catches sloppy edges.

**Detection**:
- IoU threshold (0.5 typical) decides which detections count as TP.
- **AP** = area under precision-recall curve, per class.
- **mAP** = mean of AP across classes. **Standard detection metric.**
- **COCO mAP**: averaged over IoU thresholds 0.5, 0.55, …, 0.95.

---

## 5. Loss functions

| Loss | Use case | Gist |
|---|---|---|
| **Cross-entropy** | **Classification** | `-log(p_correct_class)` |
| Binary cross-entropy | Binary / multi-label | Sigmoid + per-class CE |
| Weighted CE | Imbalanced classification | Per-class weight α_j |
| **Focal loss** | Imbalanced classification / detection | Down-weights easy examples via `(1−p_t)^γ` |
| Hinge / SVM | Classification | Correct class score should beat others by margin m |
| **MSE** | **Regression** | `(y − ŷ)²`. Sensitive to outliers |
| L1 / MAE | Regression | `|y − ŷ|`. Robust to outliers |
| **Dice loss** | **Imbalanced segmentation** | `1 − Dice`. Directly rewards mask overlap |
| Generalized Dice, Tversky | Segmentation variants | Per-class weighting / asymmetric FP/FN |
| KL divergence | Distribution matching | Used in knowledge distillation |
| InfoNCE | Contrastive SSL | (K+1)-way classification: positive vs K negatives |

**Cross-entropy vs MSE rule of thumb**: classification = CE. Regression = MSE.

---

## 6. Transformers

**Self-attention** (the heart):
- Input `X` has shape `(B, T, D)` — batch, tokens, embedding.
- Compute three projections: `Q = XW_Q`, `K = XW_K`, `V = XW_V`.
- `Attention = softmax(Q · Kᵀ / √D) · V`.
- The √D scaling is for **numerical stability** (dot products grow with D).
- **Karpathy intuition**: message passing on a fully connected graph. Every token talks to every other.
- **Q (query)** = what I'm looking for. **K (key)** = what I have. **V (value)** = what I share.

**Self-attention is permutation-invariant** → tokens are a **set** with no order → need **positional encodings**.

**Positional encodings**:
- **Sinusoidal** (original "Attention is All You Need"): `sin`/`cos` at different frequencies per dimension. Fixed, not learned.
- **Learnable** (ViT): a vector per position, trained.

**Multi-head attention**:
- Run h Q/K/V projections in parallel, each on a smaller slice of the embedding.
- Concatenate outputs, project back to D.
- Different heads can learn different relations.

**Encoder block**: MHA → add+layernorm → FFN → add+layernorm. **Residual connections.**

**Layer norm vs batch norm**: layer norm normalizes per-sample across features. Independent of batch size. **Why transformers use it.**

**Vision Transformer (ViT)**:
- Split image into patches (e.g. 16×16) → linear projection → embedding.
- Prepend a learnable **CLS token** → goes through transformer → used for final classification.
- Add positional embeddings.
- **Needs huge pretraining data** (ImageNet-21k or JFT-300M) to beat ResNets.

**DeiT**: data-efficient ViT. Uses **knowledge distillation** from a teacher CNN + heavy augmentation. Trains on ImageNet only.

**Knowledge distillation**: train a smaller student to match teacher's soft probabilities. "Dark knowledge" = info in the wrong-class probabilities (e.g. that huskies ≈ malamutes).

**Swin Transformer**:
- **Hierarchical** (multi-scale, like CNNs).
- **Window attention**: only within local windows. Reduces complexity from O(N²) to O(N).
- **Shifted windows**: alternate window placement so info flows across boundaries.

---

## 7. Segmentation

**Task** = predict a class for **every pixel**.

- **Semantic seg**: one class per pixel. Instances merged.
- **Instance seg**: also separates objects of the same class.
- **Panoptic**: combines both.

**Architectures**:
- **FCN**: fully convolutional, down-then-up.
- **U-Net**: encoder (contracting) + decoder (expanding) + **skip connections** that copy encoder features to the decoder at each level. Preserves spatial detail lost in pooling.
- **DeepLabV3**: **atrous (dilated) convolutions** insert holes in the kernel → bigger receptive field without losing resolution. **ASPP** runs atrous convs at multiple rates in parallel for multi-scale context.
- **HRNet**: keeps high-resolution stream throughout, fuses multi-resolution.
- **SegFormer**: hierarchical transformer encoder + all-MLP decoder. Overlapping patches. No positional encodings (uses 3×3 depthwise conv in FFN instead).

**Upsampling**:
- **Unlearned**: nearest-neighbour, bilinear. No params.
- **Learned**: **transposed convolution** (`nn.ConvTranspose2d`), aka "deconvolution".

**Output**:
- Network outputs a volume of shape `(H, W, C)` where C = **number of classes**.
- Each channel = probability map for one class.
- Final mask = `argmax` over the class dimension.

**Loss**: per-pixel **cross-entropy** + **Dice** (for imbalance). Often combined: `L_CE + λ · L_Dice`.

**Post-processing**: confidence thresholding, smoothing probabilities, connected-component filtering (remove tiny isolated regions).

---

## 8. Visualization / interpretability

**Interpretable** (by design): linear models, decision trees.
**Explainable** (post-hoc): Grad-CAM, LIME, SHAP. CNNs are explainable, not interpretable.

**Techniques**:
- **Filter visualization**: 1st-layer filters look like edge/color detectors.
- **Feature embedding nearest neighbours**: last-layer features → semantic similarity.
- **Occlusion sensitivity**: slide a black patch over the image; the heatmap of score drops shows where the model is "looking."
- **Saliency maps**: gradient of class score w.r.t. input pixels.
- **CAM**: requires GAP. Weighted sum of last-layer feature maps using classifier weights.
- **Grad-CAM**: generalizes CAM to **any** architecture. Weights = **gradient** of class score w.r.t. feature map. Produces a class-specific heatmap.
- **LIME**: perturb input (hide super-pixels), fit a simple model locally to the predictions. Famous "husky vs wolf — model used snow" example.

**Adversarial examples**: tiny crafted perturbations fool the model. **FGSM** uses the sign of the gradient. Even a single-pixel change can flip the prediction.

---

## 9. Object detection

**Task** = label + bounding box for each object.

**Output bbox** = `(x, y, w, h)` or transforms relative to an anchor.

**Two-stage detectors** (slower, more accurate):
- **R-CNN**: selective search (2000 region proposals) → warp → AlexNet → SVM. Slow.
- **Fast R-CNN**: CNN once on whole image → **ROI pooling** crops feature maps. Much faster.
- **Faster R-CNN**: replaces selective search with a learned **Region Proposal Network (RPN)**. End-to-end. **Anchor boxes** at each spatial position; predict objectness + bbox transforms.
- **Mask R-CNN**: adds a mask branch → instance segmentation. Uses **ROI Align** (bilinear sampling, no grid snapping).

**One-stage detectors** (faster, sometimes less accurate):
- **YOLO**: image → S×S grid. Each cell predicts B boxes (4 coords + objectness) + C class probabilities. **Single forward pass.** Modern YOLOs add SPP/SPPF, PANet, CSP blocks.
- **SSD, RetinaNet** (focal loss).

**Anchor boxes**: predefined bbox shapes (different aspect ratios). The model predicts offsets/transforms.

**NMS (Non-Max Suppression)** (post-processing):
1. Sort detections by score.
2. Take the highest.
3. Discard any other box with IoU > 0.5 against it.
4. Repeat.

**DETR**: detection as **set prediction**.
- CNN backbone → transformer encoder/decoder.
- N learned **object queries** → output N predictions in parallel.
- **Bipartite (Hungarian) matching** between predictions and ground truth for the loss.
- **No anchors. No NMS.**

---

## 10. Self-supervised learning (SSL)

**Idea**: learn from unlabeled data via a **pretext task** that generates supervisory signals from the data itself.

**Three main families**:

**1. Contrastive learning** (MoCo, SimCLR):
- Pull two augmented views of the **same image** together; push views of **different images** apart.
- Loss: **InfoNCE** (K+1-way classification: positive vs K negatives).
- **MoCo**: uses a **queue** of negatives + **momentum encoder** (EMA of the query encoder) → huge effective batch size on modest hardware.

**2. Self-distillation** (DINO):
- Student matches teacher's output distribution. Teacher = EMA of student. No negatives.
- **Multi-crop**: student sees several local crops; teacher only sees global crops. Forces local-to-global understanding.
- **Centering + sharpening** prevent mode collapse.
- DINO features are so good that **attention maps naturally segment objects** without supervision.

**3. Masked image modeling** (MAE):
- **Mask 75% of patches**, reconstruct the missing pixels.
- **Asymmetric** encoder-decoder: ViT encoder on **visible patches only**, lightweight decoder reconstructs.
- Loss: MSE in pixel space.
- Frozen features are weaker than DINO's — MAE shines after **full fine-tuning** for dense tasks (detection, segmentation).

---

## 11. Multimodal

**Tasks**:
- **VQA**: answer text questions about an image.
- **Grounding**: connect text concepts to image regions.
- **Referring expression**: bbox for one specific noun phrase.
- **Phrase grounding**: localize multiple phrases simultaneously.

**Two-stream CNNs** (video action recognition): spatial stream (RGB frames) + temporal stream (optical flow). Late fusion.

**CLIP**:
- Image encoder (ViT or ResNet) + text encoder, trained on **400M (image, caption) pairs**.
- **Contrastive loss**: pull matching pairs together, push non-matching apart (cosine similarity in shared embedding space).
- Enables **zero-shot classification** via prompts like *"a photo of a {class}"*.
- Used as backbone in Stable Diffusion, LLaVA, etc.
- Limitations: bad at counting, OCR, fine-grained tasks; brittle to prompt wording.

**GLIP**: reframes object detection as **phrase grounding**. Region-token alignment scores. Cross-modal deep fusion (image and text attend to each other in last layers). Zero-shot detection.

**SAM (Segment Anything)**:
- Foundation model for segmentation.
- Image encoder (MAE-pretrained ViT, runs once) + prompt encoder (points/boxes/text) + mask decoder.
- **Predicts 3 masks (whole / part / subpart)** to handle prompt ambiguity.
- Trained on SA-1B (1B masks, 11M images).

**BLIP-2**:
- Bridges a **frozen** image encoder + **frozen** LLM with a small **Q-Former** in the middle.
- 32 learnable query embeddings extract image features via cross-attention, fed to the LLM as soft visual prompts.
- Two-stage training: representation learning + generative learning.

---

## Quick mental hooks for the exam

| Term | One-word association |
|---|---|
| ReLU | rectifier (max with 0) |
| Sigmoid | (0, 1) |
| Tanh | (−1, 1) |
| Batch norm | across-batch normalization |
| Layer norm | per-sample, transformers |
| Dropout | random zeroing |
| ResNet | skip / residual |
| Inception | parallel filters |
| VGG | only 3×3 |
| EfficientNet | compound scaling |
| MobileNet | depthwise separable |
| Adam | adaptive per-param + momentum |
| AdamW | decoupled weight decay |
| Cosine annealing | smooth LR decay |
| Cross-entropy | classification |
| MSE | regression |
| Dice | overlap, imbalanced |
| Focal | down-weight easy |
| Hinge | margin |
| IoU | intersection / union |
| mAP | mean AP across classes |
| Recall | TP / (TP + FN) |
| Q, K, V | query / key / value |
| √D scaling | numerical stability |
| Positional encoding | order info, set→sequence |
| CLS token | classification anchor |
| Swin | windowed + shifted |
| U-Net | encoder + decoder + skips |
| ConvTranspose2d | learned upsampling |
| Atrous | dilated, holes |
| Grad-CAM | gradient-weighted heatmap |
| NMS | dedupe overlapping boxes |
| Anchor box | predefined bbox shape |
| RPN | learned region proposer |
| DETR | set prediction, no NMS |
| InfoNCE | contrastive (K+1-way) |
| MoCo | queue + momentum encoder |
| DINO | self-distillation, EMA teacher |
| MAE | mask 75%, reconstruct |
| CLIP | image-text contrastive |
| SAM | prompt → mask |
| Q-Former | image encoder ↔ LLM bridge |
