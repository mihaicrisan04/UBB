# Computer Vision and Deep Learning

Lecture 7

# Today’s agenda

- Self-attention example
- Attention is all you need
- Position encodings, embeddings
- Vision transformers
- Vision transformer
- DeiT
- Swin Transformer

# Attention intuition

https://youtu.be/XfpMkf4rD6E?t=1213 

# Self-attention

https://peterbloem.nl/blog/transformers 

# Self-attention 

- scaled dot product 
- key, value and query transformations
-  multi-head attention

# Self-attention 

- Scale the dot product
- softmax function is sensitive to very large input values
- the average value of the dot product grows with the embedding dimension k, then scale the dot product back a little to reduce the inputs of softmax function 

<!-- image -->

# Self-attention

- Scale the dot product
- Key, value and query transformation
- Each input vector has three different roles
-  the value: the vector that is used in the weighted sum that ultimately provides the output.
- the query: the input vector that corresponds to the current output, matched against every other input vector. 
- the key: the input vector that the query is matched against to determine the weight.
- Multiply each input vector by three different k-by-k parameter matrices, which gives us a different vector to act as key query and value

# Self-attention

<!-- image -->

<!-- image -->

# Self-attention

- Scale the dot product
- Key, value and query transformation
- Multi-head attention
- multiple relations are best captured by different self-attention operations.

<!-- image -->

# Self-attention

- Naïve version: apply in parralel, then concatenate	
-  apply a small number of copies of the self-attention mechanism in parallel, each with their own key, value and query transformation
- h heads, operation is h as slow
- In practice:
- each head receives low-dimensional keys queries and values. 
- If the input vector has k=256 dimensions and h=4 attention heads
- multiply the input vectors by a 256×64 matrix to project them down to a sequence of 64 dimensional  vectors
- for every head, we do this 3 times: for the keys, the queries and the values.
- 

# Self-attention

- 3h matrices of size k by k/h 
- In total: 3hk(k/h)=3k2 parameters to compute the inputs to the multi-head self-attention

<!-- image -->

# Self-attention

-  Combine the computation of the projections down to a lower dimensional representation and the computations of the keys, queries and values into three k×k matrices
- Just like in a single self attention

<!-- image -->

# Self-attention

<!-- image -->

<!-- image -->

def forward(self, x):     b, t, k = x.size()    h = self.heads     queries = self.toqueries(x)    keys    = self.tokeys(x)    values  = self.tovalues(x)     s = k // h     keys = keys.view(b, t, h, s)    queries = queries.view(b, t, h, s)    values = values.view(b, t, h, s)     # - fold heads into the batch dimension    keys = keys.transpose(1, 2).contiguous().view(b * h, t, s)    queries = queries.transpose(1, 2).contiguous().view(b * h, t, s)    values = values.transpose(1, 2).contiguous().view(b * h, t, s)     # Get dot product of queries and keys, and scale    dot = torch.bmm(queries, keys.transpose(1, 2))    # -- dot has size (b*h, t, t) containing raw weights     # scale the dot product    dot = dot / (s ** (1 / 2))     # normalize    dot = F.softmax(dot, dim=2)    # - dot now contains row-wise normalized weights     # apply the self attention to the values    out = torch.bmm(dot, values).view(b, h, t, s)     # swap h, t back, unify heads    out = out.transpose(1, 2).contiguous().view(b, t, s * h)     return self.unifyheads(out)

compute the queries, keys and values for all heads -&gt;  three vector sequences of the full embedding dimension k. 

def forward(self, x):     b, t, k = x.size()    h = self.heads     queries = self.toqueries(x)    keys    = self.tokeys(x)    values  = self.tovalues(x)     s = k // h     keys = keys.view(b, t, h, s)    queries = queries.view(b, t, h, s)    values = values.view(b, t, h, s)     # - fold heads into the batch dimension    keys = keys.transpose(1, 2).contiguous().view(b * h, t, s)    queries = queries.transpose(1, 2).contiguous().view(b * h, t, s)    values = values.transpose(1, 2).contiguous().view(b * h, t, s)     # Get dot product of queries and keys, and scale    dot = torch.bmm(queries, keys.transpose(1, 2))    # -- dot has size (b*h, t, t) containing raw weights     # scale the dot product    dot = dot / (s ** (1 / 2))     # normalize    dot = F.softmax(dot, dim=2)    # - dot now contains row-wise normalized weights     # apply the self attention to the values    out = torch.bmm(dot, values).view(b, h, t, s)     # swap h, t back, unify heads    out = out.transpose(1, 2).contiguous().view(b, t, s * h)     return self.unifyheads(out)

Cut these vectors into h chunks

<!-- image -->

def forward(self, x):     b, t, k = x.size()    h = self.heads     queries = self.toqueries(x)    keys    = self.tokeys(x)    values  = self.tovalues(x)     s = k // h     keys = keys.view(b, t, h, s)    queries = queries.view(b, t, h, s)    values = values.view(b, t, h, s)     # - fold heads into the batch dimension    keys = keys.transpose(1, 2).contiguous().view(b * h, t, s)    queries = queries.transpose(1, 2).contiguous().view(b * h, t, s)    values = values.transpose(1, 2).contiguous().view(b * h, t, s)     # Get dot product of queries and keys, and scale    dot = torch.bmm(queries, keys.transpose(1, 2))    # -- dot has size (b*h, t, t) containing raw weights     # scale the dot product    dot = dot / (s ** (1 / 2))     # normalize    dot = F.softmax(dot, dim=2)    # - dot now contains row-wise normalized weights     # apply the self attention to the values    out = torch.bmm(dot, values).view(b, h, t, s)     # swap h, t back, unify heads    out = out.transpose(1, 2).contiguous().view(b, t, s * h)     return self.unifyheads(out)

batch, seq\_len, heads, head\_dim, i.e. b, t, h, s

.transpose(1, 2) -&gt; swaps seq\_len and heads

(b, t, h, s) → (b, h, t, s)

Data is not contiguous, use .contiguous() to physically reorder the data 

.view(b * h, t, s) -&gt; merge the batch and head dimensions 

Treat each head as an independent batch element, so torch.bmm can process all heads in parallel in one call

def forward(self, x):     b, t, k = x.size()    h = self.heads     queries = self.toqueries(x)    keys    = self.tokeys(x)    values  = self.tovalues(x)     s = k // h     keys = keys.view(b, t, h, s)    queries = queries.view(b, t, h, s)    values = values.view(b, t, h, s)     # - fold heads into the batch dimension    keys = keys.transpose(1, 2).contiguous().view(b * h, t, s)    queries = queries.transpose(1, 2).contiguous().view(b * h, t, s)    values = values.transpose(1, 2).contiguous().view(b * h, t, s)     # Get dot product of queries and keys, and scale    dot = torch.bmm(queries, keys.transpose(1, 2))    # -- dot has size (b*h, t, t) containing raw weights     # scale the dot product    dot = dot / (s ** (1 / 2))     # normalize    dot = F.softmax(dot, dim=2)    # - dot now contains row-wise normalized weights     # apply the self attention to the values    out = torch.bmm(dot, values).view(b, h, t, s)     # swap h, t back, unify heads    out = out.transpose(1, 2).contiguous().view(b, t, s * h)     return self.unifyheads(out)

transpose again, so that the head dimension and the embedding dimension are next to each other, and reshape to get concatenated vectors of dimension e. 

pass these through the unifyheads (Linear(k, k)) layer for a final projection.

# Positional Encodings

- “Words” in a sentence simultaneously flows through the Transformer’s encoder/decoder stack
- The model itself doesn’t have any sense of position/order for each word
- Incorporate token order
- Unique encoding for each time-step (word’s position in a sentence).
- Distance between any two time-steps should be consistent across sentences with different lengths.
- Generalize to longer sentences without any efforts. 
- Bounded values.
- It must be deterministic.
- 

# Positional Encodings

- Frequencies decrease along the vector dimension. 
- Geometric progression from 2π to 10000⋅2π on the wavelengths.

<!-- image -->

<!-- image -->

https://kazemnejad.com/blog/transformer\_architecture\_positional\_encoding/ 

# Positional Encodings  Intuition

https://kazemnejad.com/blog/transformer\_architecture\_positional\_encoding/ 

<!-- image -->

<!-- image -->

<!-- image -->

# Positional Encodings

- “We chose this function because we hypothesized it would allow the model to easily learn to attend by relative positions, since for any fixed offset k, PEpos+k can be represented as a linear function of PEpos.”

https://kazemnejad.com/blog/transformer\_architecture\_positional\_encoding/ 

<!-- image -->

# Positional Encodings

https://kazemnejad.com/blog/transformer\_architecture\_positional\_encoding/ 

<!-- image -->

<!-- image -->

<!-- image -->

# Positional Encodings

- The distance between neighboring time-steps is symmetrical

<!-- image -->

# Attention is all you need  Step by step

<!-- image -->

# Transformer architecture

<!-- image -->

# Transformer: encoder block

- Multi head attention layer
- Layer normalization
-  Feed forward layer (applied independently to each vector) 
- Layer normalization

+ Residual connections (before the normalizations)

<!-- image -->

<!-- image -->

# Batch norm vs Layer norm

- Layer norm: just like batch norm, but applied on a different dimension 
- Does not propagate information across time dimension (self attention does this)
- Layer norm does not depend on batch size

# Layer norm

<!-- image -->

# Transformer: encoder block

- Multi head attention layer
- Layer normalization
-  Feed forward layer (applied independently to each vector) 
- Layer normalization

+ Residual connections (before the normalizations)

<!-- image -->

# Transformer: decoder block

- Input: block on the same level in the encoder and the previous level in the decoder
- Masked multi head attention

<!-- image -->

# Attention is all you need (step by step example)

https://youtu.be/Ub3GoFaUcds?t=5421 

# Other resources

- Jay Alammar series on transformers
- https://jalammar.github.io/

- Let’s build GPT from scratch
- https://www.youtube.com/watch?v=kCc8FmEb1nY 

- State of GPT
- https://www.youtube.com/watch?v=bZQun8Y4L2A 
- 

# An Image is Worth 16x16 Words: Transformers for Image Recognition at Scale

Vision transformer

Official implementation in JAX: https://github.com/google-research/vision\_transformer#vision-transformer 

# Vision transformer

<!-- image -->

# Vision transformer

<!-- image -->

The image x ∈ R H×W×C is transformed into a sequence of flattened 2D patches xp ∈ RN×(P2 ·C) 

- (P, P) is the resolution of each image patch
- sequence length: N = HW/P2
- 

Flatten the patches and map to D dimensions with a trainable linear projection

- constant latent vector size D through all of its layers
- 

# Vision transformer

<!-- image -->

<!-- image -->

https://github.com/google-research/vision\_transformer/blob/main/vit\_jax/models\_vit.py 

# Vision transformer

<!-- image -->

# Position embeddings

- A unique learnable vector pi ∈ ℝD is added to each patch embedding before entering the Transformer encoder.
- Embeddings are learned from scratch during training, not hand-crafted.
- Each position gets its own independent vector - no inductive bias about proximity.
- The special [CLS] token also receives a positional embedding at index 0.
- The original ViT paper tested 1D, 2D, and relative positional encodings and found no significant difference in performance. 1D was kept for simplicity. 
- 

# Position embeddings

- Drawbacks?
- At test time, if image resolution changes, the number of patches N changes.
- Positional embeddings must be interpolated (bicubic), which causes a performance drop. 

<!-- image -->

# Position embeddings visualisation

<!-- image -->

for each patch position i, computes the cosine similarity between that position's embedding and all other positions' embeddings

pos\_embed shape: (1, 197, D) 

<!-- image -->

# Position embeddings visualisation

<!-- image -->

reshape the result into a 14×14 grid (matching the spatial layout of the patches) and plot it as a heatmap

<!-- image -->

# Position embeddings visualisation

<!-- image -->

the bright spot in each cell tends to be localised around the cell's own position, and similarity falls off with distance.

# Position embeddings

- Even though ViT uses 1D learnable positional embeddings (no explicit 2D structure), the model learns 2D spatial structure: nearby patches develop similar embeddings.
- There's also a visible row/column pattern (grid-like structure in some cells) -&gt; the model learns both horizontal and vertical proximity.

# Vision transformer

<!-- image -->

0, 1, 2, …

Position embeddings are added to the patch embeddings to retain positional information. 

(standard learnable 1D position embeddings)

# Vision transformer

<!-- image -->

<!-- image -->

https://github.com/google-research/vision\_transformer/blob/main/vit\_jax/models\_vit.py 

# Vision Transformer

The Transformer encoder consists of alternating layers of multiheaded self-attention and MLP blocks.

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# Vision Transformer 

- Hidden size D embedding dimension: the size of every token vector as it flows through the entire Transformer.
- after the self-attention, there is a 2-layer MLP (equations 3 from the paper). The MLP size is the dimension of the hidden layer inside that MLP (4x)

<!-- image -->

BiT – modified Resnet architecture

Imagenet: 1.2M images, 1k classes

ViT models perform worse than ResNets    

B = Base 

L = Large 

H = Huge 

/32, /16, /14 is patch size; smaller patch size is a bigger model (more patches)

<!-- image -->

BiT – modified Resnet architecture

ImageNet-21k: 14M images, 21k categories

pretrain on ImageNet-21k and fine-tune on ImageNet 

big ViTs match big ResNets

B = Base 

L = Large 

H = Huge 

/32, /16, /14 is patch size; smaller patch size is a bigger model (more patches)

<!-- image -->

BiT – modified Resnet architecture

JFT - 300M internal Google dataset with 300M images

pretrain on JFT  and fine-tune on ImageNet 

large ViTs outperform large ResNets

B = Base 

L = Large 

H = Huge 

/32, /16, /14 is patch size; smaller patch size is a bigger model (more patches)

# DeIT

Training data-efficient image transformers &amp; distillation through attention

https://arxiv.org/abs/2012.12877 

# Knowledge distillation

- Teaching a student intern:
- Standard training: Give them the documentation and let them figure it out alone
- Distillation: Have an experienced expert work alongside them, sharing their reasoning and intuitions, not just the final answers
- Knowledge distillation is the machine learning version of the second approach.
- large, powerful teacher model (already well-trained)
- train a smaller or weaker student model to mimic the teacher
- The student learns from both the ground-truth labels and the teacher's behaviour
- 

# Knowledge distillation

<!-- image -->

https://pub.towardsai.net/a-gentle-introduction-to-knowledge-distillation-6240bf8eb8ea 

Step 1: Train a teacher model on images and ground-truth labels (cross entropy loss)

Step 2: Train a student model to match predictions from the teacher

KL divergence loss

(knowledge distillation loss)

You can also use standard CE loss

<!-- image -->

# “Dark knowledge”

- The teacher's probability distribution carries hidden knowledge:
- Inter-class similarity: the model has learned that Huskies and Malamutes are visually close
- Uncertainty information: when the teacher is unsure, the student learns to be unsure too
- Error structure: the pattern of which classes get confused encodes real visual relationships
- Introduced by G. Hinton. Idea: the small probabilities on wrong classes are not noise, they are meaningful
- This is sometimes called the "dark knowledge" in the teacher: information that exists in the model but never appears in the training labels.

<!-- image -->

# DeIT Data-efficient Image Transformers

1. Train a teacher CNN on ImageNet
2. Train a student ViT to match ImageNet predictions from the teacher CNN and match GT labels

https://arxiv.org/abs/2012.12877 

<!-- image -->

# DeIT Data-efficient Image Transformers

https://arxiv.org/abs/2012.12877 

Predicted class scores; should match labels

Use CE loss

Predicted class scores; should match teacher output

# Soft-label vs Hard-label distillation

- Softmax with temperature: 

<!-- image -->

<!-- image -->

# DeiT – augmentation and regularization

- Beyond distillation, DeiT uses aggressive data augmentation and regularization to prevent overfitting on ImageNet:
- RandAugment: random color, shear, rotation transforms
- Mixup
- CutMix 
- Random Erasing
- Label Smoothing
- Stochastic Depth

# DeiT – augmentation and regularization

- Beyond distillation, DeiT uses aggressive data augmentation and regularization to prevent overfitting on ImageNet:
- RandAugment: random color, shear, rotation transforms
- Mixup: blend two images and interpolate their labels
- CutMix : paste a patch from one image onto another
- Random Erasing: randomly mask out a region of the image
- Label Smoothing: soften one-hot targets slightly
- Stochastic Depth: randomly drop transformer layers during training
- 

# DeIT Data-efficient Image Transformers

<!-- image -->

# DeIT Data-efficient Image Transformers

<!-- image -->

<!-- image -->

# DeIT Data-efficient Image Transformers

- Always check Real and V2 columns, not just ImageNet top-1
- DeiT-B distilled matches EfficientNet-B5 at 3x the params but higher throughput
- At 384² DeiT surpasses every CNN in the table
- The * RegNet models are the actual teacher models used: DeiT matches its own teacher
- Resolution boosts accuracy but kills throughput: EfficientNet-B7 runs at 55 img/s vs DeiT's 290 img/s

# DeIT Data-efficient Image Transformers

- You no longer need Google-scale data to train a competitive ViT
- Standard academic GPU clusters are sufficient
- Transformers can match CNNs with the right training strategy

# Swin Transformer

SwinTransformer: Hierarchical Vision Transformer using Shifted Windows

 

# SWIN Transformer

- CNNs – hierarchical architecture
- Decrease resolution and increase channels as you go deeper in the network
- 
- ViT – isotropic architecture
-  all blocks have same resolution and number of channels

<!-- image -->

# SWIN Transformer

- ViT and DeiT bottlenecks
- Every patch token attends to every other patch token: global self-attention
- For an image of N patches, this costs O(N²) in memory and computation
- A 224×224 image with 16×16 patches = 196 tokens: manageable
- A 512×512 image with 16×16 patches? = 1,024 tokens: 25× more expensive
- This makes ViT impractical for high-resolution vision tasks like object detection and segmentation

# SWIN Transformer

- Compute attention only within local windows, not globally
- Use a hierarchical structure like CNNs (feature maps at multiple scales)
- Connect windows across layers using a shifting mechanism

# SWIN Transformer

- With H x W grid of tokens, each attention matrix is quadratic in image size H2x W2
- Swin transformer computes self-attention locally within M non-overlapping windows that partition an image
- Split image into M windows (M=4 in this example) 
- Only compute attention within each window

<!-- image -->

# SWIN Transformer

- Global Attention
- Feature map: h × w × C
- Global attention: every patch attends to every other patch
- Total patches: hw
- Complexity: O((hw)²): quadratic in image size
- Window-Based Attention
- Divide into non-overlapping M×M windows
- Number of windows: hw/M²
- Each window has M² patches: attention stays local
- Complexity: O(hw) — linear in image size
- 

# SWIN transformer

| Operation           | Global   | Window   |
|---------------------|----------|----------|
| Q, K, V projections | 3hwC²    | 3M²C²    |
| QKᵀ                 | (hw)²C   | M⁴C      |
| Softmax(QKᵀ)V       | (hw)²C   | M⁴C      |
| Final projection    | hwC²     | M²C²     |

# SWIN Transformer

- A patch in one window cannot attend to a patch in another window
- Problem: imagine a dog that spans two windows: the left half and the right half are never compared. The model cannot reason about objects that cross window boundaries, and it cannot build up global context.
- Solution	
- SHIFTED WINDOWS
- 

# SWIN Transformer

- (a) regular window partitioning VS  (b) shifted window partitioning for a 4 ✕ 4 feature map with 2 ✕ 2 windows of size 2 ✕ 2 (M = 2)
- (b)The window grid has been shifted by 1 patch (M/2 = 1) toward the top-left

<!-- image -->

# SWIN Transformer

- Shifting creates partial windows at the edges: smaller than M×M
- Number of windows grows from ⌈h/M⌉ × ⌈w/M⌉ to (⌈h/M⌉ + 1) × (⌈w/M⌉ + 1)
- More windows + irregular sizes = inefficient computation
- Naïve fix
- Pad partial windows back up to M×M
- Simple, but adds unnecessary computation on padding tokens
- Real fix:
- Cyclically roll the feature map toward the top-left. This fills the partial edge windows using pixels wrapped from the opposite side
- All windows are now exactly M×M: no padding needed

# SWIN Transformer – cyclical shift

<!-- image -->

# SWIN Transformer – cyclical shift

- The entire feature map has been rolled toward the top-left by 1 patch
- Region 0 (was top-left corner): wrapped to bottom-right
- Regions 1, 2 (were top row): wrapped to bottom row
- Regions 3, 6 (were left column): wrapped to right column

- Region 8 is now attending alongside regions 6, 2, 0 in the same window even though they are not actually spatial neighbours in the original image.

<!-- image -->

# SWIN Transformer – cyclical shift

- Wrapped pixels are not real spatial neighbours
- Allowing them to attend to each other would create false relationships
- Solution: apply an attention mask so wrapped regions cannot attend to each other
- After attention, the feature map is rolled back to its original position

# SWIN Transformer - cyclical shift

# SWIN Transformer – cyclical shift

- If a connection is valid, the mask value is 0, meaning it does not affect the SoftMax computation. 
- If a connection is artificial, the value of the mask will be -100 (or another very small value), causing the SoftMax function to assign it a negligible probability

# SWIN Transformer

- The window-based self-attention module lacks connections across windows, which limits its modeling power. 
- No interaction with tokens outside the window
- 
- Solution: alternate between normal and shifted in alternative transformer blocks in the architecture 

# SWIN Transformer

<!-- image -->

Divide image into 4x4 patches and project to C dimensions

https://arxiv.org/abs/2103.14030 

# SWIN Transformer

- where ẑl and zl denote the output features of the (S)WMSA module and the MLP module for block l

<!-- image -->

<!-- image -->

# SWIN Transformer

<!-- image -->

Divide image into 4x4 patches and project to C dimensions

https://arxiv.org/abs/2103.14030 

Merge patches in neigh. 2x2, patches are 8x8

SWIN transformer block

# SWIN Transformer

https://arxiv.org/abs/2103.14030 

<!-- image -->

<!-- image -->

<!-- image -->

“The first patch merging layer concatenates the features of each group of 2×2 neighboring patches, and applies a linear layer on the 4C-dimensional concatenated features.

This reduces the number of tokens by a multiple of 2×2=4 (2× down sampling of resolution), and the output dimension is set to 2C”

Linear projection (1x1 conv)

# SWIN Transformer

- Other details
- Cyclic shifting and masked attention
- Encodes relative position between patches when computing attention

- 
- 
- 

<!-- image -->

<!-- image -->