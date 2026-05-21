# CVDL Study Guide — Labs 3, 4, 5

A plain-English walkthrough of the concepts in:
- Lab 3 — Convolutional Neural Networks
- Lab 4 — Semantic Segmentation
- Lab 5 — Transformers (Vision Transformers + Attention)

No heavy math. Focus is on **what each thing is** and **why it exists**.

---

## Lab 3 — Convolutional Neural Networks (CNNs)

### The big idea
A CNN is a neural network designed for **images**. Instead of treating an image as a flat list of pixels, it slides small filters across the image and learns which **visual patterns** are useful.

Why this matters: nearby pixels in an image are related (a cat's ear is a few pixels of a similar color, not random pixels scattered everywhere). CNNs exploit this **locality**.

### The convolution operation
A **filter** (also called **kernel**) is a small matrix (e.g. 3×3) of numbers. You slide it across the image; at each position you compute element-wise multiplication between the filter and the image patch beneath it, then sum the result. That single number goes into the **output** (also called the **activation map** or **feature map**).

The key hyperparameters:
- **Filter size (F)** — how big the kernel is (e.g. 3×3, 5×5). Usually odd.
- **Padding (P)** — extra zero pixels added around the image so the filter can reach the borders.
- **Stride (S)** — how many pixels the filter jumps between applications. Stride > 1 shrinks the output.
- **Number of filters (k)** — how many different patterns the layer tries to detect. Each filter produces its own output channel.

Output size formula:
```
W_out = (W_in - F + 2P) / S + 1
```
(same for height)

> Intuition: each filter is a "pattern detector." After training, one filter might detect vertical edges, another might detect a furry texture, another might detect a curve shaped like an eye.

### Classic image filters (manually designed, not learned)
- **Mean filter** — averages neighbors. Result: **blurry** image (low-pass, removes high-frequency detail).
- **Gaussian filter** — weighted average where center weights more. **Smoother blur** than mean.
- **Sobel filter** — detects **edges**. Sobel-X catches vertical edges (horizontal gradients), Sobel-Y catches horizontal edges. It's a high-pass filter.

The point of doing these by hand: a CNN ends up **learning filters like these automatically** in its early layers.

### Pooling
**Pooling** reduces the spatial size (H × W) of feature maps. It has **no learnable parameters**.

- **Max pooling** — keeps the largest value in each window. Most common.
- **Average pooling** — keeps the average value in each window.

Why pool? It makes the network **cheaper**, more **robust to small shifts**, and grows the **receptive field** (how much of the original image one neuron "sees").

### Building a CNN (in PyTorch)
You define your network by extending `torch.nn.Module`:
- `__init__` — declare the layers (conv, pool, linear, etc.).
- `forward` — define how data flows through them.

The typical pattern is `Conv → ReLU → Pool` blocks repeated several times, then **flatten** and pass through one or more **fully-connected (linear)** layers ending in the class predictions.

### Training loop (the recipe)
```
for epoch in epochs:
    for batch in dataloader:
        optimizer.zero_grad()   # clear old gradients
        outputs = model(inputs) # forward pass
        loss = criterion(outputs, labels)
        loss.backward()         # compute gradients
        optimizer.step()        # update weights
```

Vocab:
- **Sample** — one training example.
- **Batch** — a group of samples processed together before updating weights.
- **Epoch** — one full pass through the entire training dataset.
- **Loss / criterion** — measures how wrong the prediction is.
- **Optimizer** — algorithm that uses gradients to update weights (e.g. SGD, Adam).
- **Learning rate** — how big a step the optimizer takes. Too big → unstable. Too small → slow.
- **Scheduler** — adjusts the learning rate during training (e.g. lower it over time).

### Transfer learning
Training a CNN from scratch needs **a ton of data** and compute. Most of the time, you instead:
1. Take a model **pre-trained** on a huge dataset (usually ImageNet, e.g. ResNet18).
2. **Reuse** its learned filters (they already detect edges, textures, shapes).
3. **Adapt** it to your task by either:
   - **Linear probing / feature extraction** — freeze the backbone (`requires_grad = False`) and only train a new final classifier.
   - **Fine-tuning** — let the whole network keep learning, usually with a smaller learning rate.

Important: if the original model expects specific preprocessing (a particular size, mean/std normalization), **you must use the same** preprocessing.

Also important: `model.train()` vs `model.eval()` — some layers (dropout, batch norm) behave differently in training vs evaluation. Don't forget to switch modes.

### Hyperparameter search (sweeps)
Instead of manually tweaking values, you run many trainings with different settings:
- **Grid sweep** — try every combination from a small set. Easy, but explodes with more params.
- **Random sweep** — sample combinations randomly. More efficient when compute is limited.

Tools like **wandb Sweeps** automate this and let you compare runs.

### Visualizing what the CNN learned

Four techniques worth knowing:

1. **Nearest neighbours in feature space** — take the features from a layer near the end, and find the closest training images to your query. If neighbours share semantic class, the network has learned meaning. If they only share color/background, it's relying on shortcuts.

2. **Occlusion sensitivity** — cover small patches of the image with a black square and see how the prediction drops. If hiding the cat's face crashes the score, the model is actually looking at the face. Produces a **heatmap** of importance.

3. **Top-activating patches** — for a chosen neuron/channel in a chosen layer, find which input image patches make it fire strongest. Reveals what that neuron has learned to detect (eye? wheel? grass?).

4. **Grad-CAM** — uses the **gradient** of the predicted class score w.r.t. a feature map to weight channels, then sums them up. Produces a **class-specific heatmap** showing where the network "looked" to make its prediction.

---

## Lab 4 — Semantic Segmentation

### The big idea
Image classification: "this image contains a cat" (one label per image).
**Semantic segmentation**: "this pixel is cat, this pixel is grass, this pixel is sky" — **one label per pixel**.

The output is a **mask** the same size as the input image, where each pixel stores a class index.

### Datasets and DataLoaders
With image classification, you can sometimes load everything into memory. With segmentation you usually can't — images and masks are big. So PyTorch splits the work:

- **Dataset** — answers "given index `i`, give me sample `i`." Defines how to load **one** image + its mask from disk.
- **DataLoader** — answers "how do I iterate efficiently?" Handles **batching**, **shuffling**, **parallel loading** via worker processes.

A custom `Dataset` must implement:
- `__init__` — set up paths, transforms.
- `__len__` — return total number of samples.
- `__getitem__(idx)` — load and return sample `idx` (image + mask tuple).

### Paired augmentation (the tricky part)
With classification, if you flip the image horizontally, the label doesn't change. **With segmentation, the mask must be flipped too**, in exactly the same way.

Any **geometric transformation** (crop, flip, rotate, warp) must be applied **identically** to image and mask. Otherwise pixels and labels get misaligned and training breaks. Use `torchvision.transforms.v2` for synchronized transforms, or write your own paired augmentations.

### U-Net architecture
U-Net is a **fully convolutional network** shaped like a U:

```
ENCODER (contracting)         DECODER (expanding)
   image                          mask
    |                              ^
   conv-conv → pool                |
        |                          |
       conv-conv → pool   ───→  upsample → conv-conv
            |                       |
           conv-conv → pool ─→ upsample → conv-conv
                |                   |
              bottleneck ───────────┘
```

Two paths:
- **Contracting (encoder)** path — convolutions + max pooling. Captures **context** (what's in the image). Shrinks spatial size, grows channels.
- **Expanding (decoder)** path — **upsampling** + convolutions. Restores resolution so the output mask matches the input image. Enables **precise localization**.

The **skip connections** are the secret sauce: at each level the encoder's feature map is **copied across** and concatenated to the decoder's feature map at the same level. This brings back **high-resolution spatial detail** that pooling would otherwise lose.

The final output has **C channels** where C = number of classes. Channel c is a probability map for class c.

### Upsampling
You shrunk the image with pooling — now you have to grow it back. Two families:

- **Unlearned upsampling** (fixed, no params):
  - **Nearest neighbour** — copy each pixel into a bigger block.
  - **Bilinear interpolation** — smoothly blend between pixels.
  Fast, simple, can't adapt to the task.

- **Learned upsampling** (trainable params):
  - **Transposed convolution** (also called "deconvolution") — like convolution in reverse; learns its own upsampling pattern. In PyTorch: `nn.ConvTranspose2d`.

Output size for transposed conv: `W_out = (W_in - 1) · S - 2P + F`

### The decoder block
A typical decoder block:
1. Upsample the input (transposed conv → batch norm → ReLU).
2. **Crop** the corresponding encoder activation map to match size (use `CenterCrop`).
3. **Concatenate** along the channel axis (this is the skip connection).
4. Apply a regular conv block.
5. Pass to the next decoder level.

At the very end, a **1×1 convolution** with `C` channels turns the final feature map into per-pixel class scores.

### Loss functions for segmentation

- **Cross-entropy (CE) loss** — applied **per pixel** independently. Stable, easy baseline. Works well when classes are roughly balanced.

- **Dice loss** — measures **overlap** between predicted and ground-truth mask, like IoU but differentiable.
  ```
  Dice = 2 · Σ(p · g) / (Σp + Σg)
  L_Dice = 1 - Dice
  ```
  Useful when classes are **imbalanced** (e.g. small foreground object in a big background). CE alone might just predict "background" everywhere and still get high pixel accuracy.

- **Combined loss** — `L = L_CE + λ · L_Dice`. Best of both: stable optimization from CE + good overlap from Dice.

- **Optional: boundary / Hausdorff / focal losses** — focus on edges and contours for sharper boundaries.

### Evaluation metrics
Notation: `n_ij` = number of pixels predicted as class `j` but actually class `i`. `t_i` = total pixels truly of class `i`. `C` = number of classes.

- **Pixel accuracy** — what % of pixels are classified correctly. Misleading on imbalanced data (a model predicting all background can score 95% if 95% of pixels are background).

- **IoU (Intersection over Union)** for a class = `area of overlap / area of union` between predicted and true mask of that class.

- **Mean IoU (mIoU)** — average IoU across all classes. The **standard segmentation metric**. Ideal value is 1; under 0.6 is usually bad.

- **Frequency-weighted IoU (fIoU)** — like mIoU but each class is weighted by how often it appears. Smoother for very imbalanced datasets.

> Important: numbers don't tell the whole story. Always **visualize predicted masks overlaid on the input image** — boundary quality, missed small objects, and systematic errors are often invisible in metrics.

### Post-processing
The raw output is per-pixel probabilities, not a final mask. You usually take **argmax** to pick the most likely class per pixel — but this throws away **confidence info**.

Post-processing happens **after** training, without modifying the model:
- **Confidence thresholding** — set low-confidence pixels to "background" or "unknown."
- **Spatial smoothing** — average probabilities in small neighbourhoods before argmax.
- **Connected-component filtering** — remove small isolated regions that are likely noise (use OpenCV's connected components).

---

## Lab 5 — Vision Transformers and Attention

### The big idea
CNNs assume **locality** (nearby pixels matter most) and **translation equivariance** (a cat is a cat anywhere in the image). These are called **inductive biases**, and they help CNNs learn from less data.

Transformers throw those assumptions away. They model **global relationships**: every part of the image can look at every other part directly. They need more data to learn the same things, but at scale they can represent more flexible relationships.

### From image to tokens
A Vision Transformer (**ViT**) doesn't operate on raw pixels. Steps:
1. **Patchify** — split the image into small fixed-size patches (e.g. 16×16).
2. **Project** each patch into an embedding vector (a learned linear layer).
3. (Optionally) prepend a special **CLS token** — a learnable embedding that will aggregate global info and is used for the final classification.
4. **Add positional encodings** (more on these below).

You end up with a tensor of shape `(B, T, D)`:
- B = batch size
- T = number of tokens (patches + CLS)
- D = embedding dimension

### Self-attention — the core operation

For each token, the model computes **three vectors**:
- **Query (Q)** — "what am I looking for?"
- **Key (K)** — "what kind of info do I have?"
- **Value (V)** — "what info will I share if asked?"

These come from three separate learned linear projections of the input:
```
Q = X · W_Q
K = X · W_K
V = X · W_V
```

The **attention scores** measure how well each token's query matches every other token's key (dot product):
```
S = Q · K^T / √D     (scaled for numerical stability)
```
`S` has shape `(B, T, T)` — for each token, a similarity score with every other token.

A **softmax** over the last dimension turns scores into weights that sum to 1:
```
A = softmax(S)
```

Then the output for each token is a **weighted average** of all values:
```
Y = A · V
```

> **Karpathy's intuition**: self-attention is **message passing on a fully connected graph**. Every token is a node; every node talks to every other node. The query says what it wants; the key says what's on offer; the value is the content actually shared.

### Multi-head attention
Instead of one set of Q/K/V projections, you run **several in parallel** (e.g. 8 heads), each on a smaller slice of the embedding. Then concatenate their outputs. Different heads can learn to attend to different things (e.g. one head tracks color similarity, another tracks shape).

### Why positional encodings?
Self-attention treats tokens as a **set**, not a sequence. Swap two patches in the input — the output is the same! That's a disaster for images, where position obviously matters.

**Positional encodings** inject "where each token came from" into the embeddings. They're **added** to the token embeddings: `Z = X + PE`.

**Sinusoidal positional encodings** (from "Attention is All You Need"):
```
PE(pos, 2i)   = sin(pos / 10000^(2i/d))
PE(pos, 2i+1) = cos(pos / 10000^(2i/d))
```
where `pos` is the token index and `i` indexes embedding dimensions.

Intuition: each dimension is a wave at a different frequency. Low dimensions vary slowly across positions (give coarse position info); high dimensions oscillate fast (give fine position info). Together they create a unique fingerprint for each position.

### Attention visualization

#### Single-layer CLS attention
The CLS token's attention row tells you which patches the CLS attended to in that layer.
1. Pick the row of the attention matrix for CLS.
2. Drop the CLS-to-CLS entry.
3. Reshape the remaining values back into a patch grid.
4. Upsample to image size.
5. Overlay as a heatmap.

Early layers tend to attend broadly/locally; deeper layers focus on semantic regions.

#### Attention rollout
A single layer doesn't show the full information flow because attention compounds across layers. **Attention rollout** composes attention across all layers:
1. Get attention matrix `A_l` for each layer.
2. Add identity (`A_l + I`) to account for residual connections.
3. Normalize rows so each row sums to 1 (it becomes a transition matrix).
4. Multiply all of them: `R = A_L · ... · A_2 · A_1`.
5. The CLS row of `R` tells you each patch's total contribution to the final classification.

Rollout maps are usually **smoother**, **more global**, and **more semantically meaningful** than single-layer maps.

### Fine-tuning a ViT

Three strategies, by cost and aggressiveness:

- **Linear probing** — freeze the backbone, train only the new classification head. Cheap, fast. Useful when the pretrained features are already good and your dataset is small.

- **Full fine-tuning** — unfreeze everything and train with a **smaller learning rate**. More expressive, often better accuracy, but risks **overfitting** on small datasets and is much more expensive.

- **LoRA (Low-Rank Adaptation)** — middle ground. Freeze the original weights. For chosen weight matrices (typically Q and V projections in attention), add a small trainable update `ΔW = B · A` where `A` is `(r, D)` and `B` is `(D, r)`, with **rank r ≪ D** (e.g. r=4 or 8). The update is **low-rank**, so it has very few parameters. Train only A, B, and the classification head.

Why LoRA matters: huge models become trainable on modest hardware because you're updating maybe **1% or less** of the parameters. Often matches full fine-tuning surprisingly closely.

> Note: in lab 5 the LoRA section is struck-through, suggesting it may be optional / not required.

---

## Cross-cutting concepts (show up in every lab)

- **Overfitting** — model memorizes training data, performs poorly on unseen data. Combat with: more data, augmentation, regularization, early stopping, freezing.
- **Backbone vs head** — backbone = the big pretrained feature extractor; head = the small task-specific layer(s) on top.
- **Freezing weights** — `requires_grad = False` stops a tensor from being updated.
- **`torch.no_grad()`** — context manager to skip gradient tracking entirely; used during evaluation/inference.
- **train() vs eval()** mode — affects layers like dropout, batch norm.
- **Inductive bias** — built-in assumptions about the problem. CNNs have strong inductive biases (locality, translation equivariance); transformers have weak ones.

---

When you're ready, come back and I can:
- answer specific questions on any concept here
- prepare a fill-in-the-blank test on labs 3-4-5 (just say which lab(s) and how many questions)
