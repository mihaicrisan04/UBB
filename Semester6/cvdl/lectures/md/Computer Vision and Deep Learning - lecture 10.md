# COMPUTER VISION AND DEEP LEARNING

LECTURE 8

# Supervised. Unsupervised. Self-supervised learning

-  SUPERVISED LEARRNING: models are trained on a dataset that includes both input data and corresponding labels
- Imagenet labelling (Amazon Mechanical Turk)
- Labelling started in July 2008 and ended in April 2010
- It took 49K workers from 167 countries filtering and labelling over 160M candidate images
- Each of the 14 million images labelled three times.
- UNSUPERVISED LEARNING: analyses unlabelled datasets to identify hidden patterns, structures, or relationships without human intervention
- SELF-SUPERVISED LEARNING: system learns to understand and interpret data by teaching itself
- 
- 
- 

# Supervised. Unsupervised. Self-supervised learning

<!-- image -->

# Self-supervised learning

-  Define a pretext task that generates supervisory signals from the data itself
- The network learns representations by solving the pretext task, and these representations transfer to downstream tasks
-  Idea: if a model can predict missing or transformed parts of data, it learned something meaningful about the data's structure.
- 
- 
- 
- 

# Self-supervised learning

1. Contrastive learning
2. Self-distillation 
3. Masked image modeling (MIM)
4. 
5. 
6. 

# Self-supervised learning

1. Contrastive learning
2. pull together representations of different views of the same image (positives), push apart representations of different images (negatives)
3. learn “same” vs “different” representations
4. The typical loss is InfoNCE: softmax cross-entropy where the positive pair is the correct class and all negatives are incorrect classes
5. 
6. 

# Self-supervised learning

1. Contrastive learning
2. Self-distillation 
3. usually, use two views of the same image and train the model such that their representations match
4. NO is no explicit push-apart force on negatives
3. Masked image modeling (MIM)
6. 
7. 
8. 

# Self-supervised learning

1. Contrastive learning
2. Self-distillation 
3. Masked image modeling (MIM)
4. The model receives an image with parts removed and must predict the missing content
5. Reconstructive learning signal: the model must understand enough about visual structure to fill in the gaps

	

- 
- 
- 

# Contrastive learning

- Momentum Contrast for Unsupervised Visual Representation Learning (MoCO)
- https://arxiv.org/pdf/1911.05722 

# Contrastive Learning

<!-- image -->

# Contrastive learning (CL)

- Pull representations of two augmented views of the same image together, push representations of different images apart
- The model solves a discrimination task: given a query, identify its matching key among a set of distractors.
- Works if distractors are hard enough
- E.g. K negatives drawn randomly from ImageNet's ~1000 classes,  K needs to be large enough that most classes appear at least once in the negative set. 
- At K=256 -&gt; many classes are absent and the task is underspecified
- 

# InfoNCE (Noise Contrastive Estimation) loss

-  Frames representation learning as a K+1-way classification problem
-  Given a query q, one positive key k⁺ (another view of the same image), and K negative keys k₁…k\_K (views of different images), the loss classifies which key is the positive

<!-- image -->

# InfoNCE loss

-  Frames representation learning as a K+1-way classification problem
-  Given a query q, one positive key k⁺ (another view of the same image), and K negative keys k₁…k\_K (views of different images), the loss classifies which key is the positive
- Minimizing this loss simultaneously maximizes the similarity between query and positive and minimizes similarity between query and all negatives

<!-- image -->

# Contrastive learning

-  CL can be seen as building a discrete dictionary over a on high-dimensional continuous input (images)
-  Dynamic dictionary: 
- keys are randomly sampled 
- key encoder evolves during training
- Hypothesis: good features can be learned by a large dictionary that covers a rich set of negative samples, and the encoder for the dictionary keys is kept as consistent as possible despite its evolution
- 

# Patterns in contrastive learning

<!-- image -->

# Patterns in CL End to end

-  Uses the samples in the current mini-batch as the dictionary, so the keys are consistently encoded 
-  The model needs a very large batch size of negative samples which is memory expensive

<!-- image -->

# Patterns in CL Memory bank

-  A memory bank contains the feature representation for all images in the dataset
-  Feature representations for each image are exponential moving average calculated in prior epochs
-  The memory bank needs to be updated and replaced
-  Drawback: it needs to regularly update the feature representations in the memory bank which can be computationally expensive.
- 

<!-- image -->

<!-- image -->

# Patterns in CL Momentum encoder. MoCo

-  Dictionary is maintained as a queue of samples -&gt; allows the reuse of keys in the immediate preceding mini-batches
- Dictionary can be much larger than the typical mini-batch size
- Samples in the dictionary are progressively replaced: enqueue the current mini-batch and remove the oldest one
- Oldest keys are most outdates and less consistent with the newer ones

# MoCo

-  Queues can increase the size of the dictionary, but keys update becomes intractable with backprop (the gradient should flow to all samples in the queue)
-  Use momentum update
- 
- 
- Only θq is updated by backprop. θq evolves more smoothly (keys in the queue are encoded by different encoders – different mini-batches – their difference is not that big)
- Use a large momentum m = 0.999
- 

<!-- image -->

<!-- image -->

# MoCo Pretext task

-  a query and a key are a positive pair if they originate from the same image, and otherwise as a negative sample pair
- Take two random “views” of the same image under random data augmentation to form a positive pair
- The data augmentation setting: a 224x224-pixel crop is taken from a randomly resized image, and then undergoes random color jittering, random horizontal flip, and random grayscale conversion
- Queries and keys are encoded by two different encoders fq and fk

<!-- image -->

Key encoder starts as an exact copy of the query encoder.

<!-- image -->

Two independent random augmentations of the same batch. x\_q and x\_k look different but contain the same images. One will produce the query, one the positive key.

<!-- image -->

FW pass. N – images, C – dimensionality each

Cut the gradient through the key encoder

<!-- image -->

Batched matrix multiply between query and its corresponding positive key

q.view(N,1,C) -&gt; (N, 1, C) 

k.view(N,C,1) -&gt; (N, C, 1)

The bmm result shape: (Nx1x1) -&gt; (Nx1)

Each of the N images gets one scalar: the dot product between its query and its own positive key. 

High value = good alignment.

<!-- image -->

Matrix multiply between all queries and all queued keys. 

Shape: (NxC) · (CxK) = (NxK). K queue size

Every query is compared against every key in the queue. 

<!-- image -->

Concatenate positive and negative logits along the key dimension

 Shape: (N x (1+K))

 The positive is at index 0

<!-- image -->

labels -&gt; the correct answer is always at index 0 (the positive). 

logits/t -&gt; apply the temperature before the softmax inside CrossEntropyLoss. 

Low t sharpens the distribution and focuses gradients on hard negatives

<!-- image -->

Gradients flow only through the query encoder. f\_k was detached. Only f\_q's parameters are updated.

EMA update of the key encoder

m = 0.999

f\_k slowly and consistently evolving, to prevent the inconsistency that would occur if f\_k changed as fast as f\_q

<!-- image -->

FIFO queue update. 

The N fresh keys from this batch are added to the front, and the N oldest keys are removed from the back.

 The queue always holds exactly K keys, all produced by recent versions of f\_k. MoCo gets 65536 negatives at batch size 256 and queue size the 256 previous batches

# MoCo  Results

<!-- image -->

# MoCo Object detection

<!-- image -->

# MoCo

-  MoCo -&gt; a strong pretrained backbone but doesn’t require afford the large-batch compute as other CL methods
- The contrastive objective directly optimizes instance discrimination, each image gets a unique identity in the embedding space -&gt; MoCo features well-calibrated for distance-based tasks
- Use MoCo for image retrieval and similarity search, where cosine similarity between features is meaningful 
- MoCo falls short at dense prediction. The final global average pool vector captures what is in the image but not where. 

# Self-distillation 

Emerging Properties in Self-Supervised Vision Transformers (DINO)

https://arxiv.org/abs/2104.14294 

# DINO Knowledge distillation (KD)

- Knowledge distillation is a learning paradigm in which a student network gθs is trained to match the output of a given teacher network gθt
- Ps(x) and Pt(x), student and teacher predicitions (probabilities) for an image x
- 
- 

-  Given a fixed teacher network gθt , DINO learns to match the student and teacher distributions by minimizing the cross-entropy loss w.r.t. the parameters of the student network gθs
- 
- 

<!-- image -->

<!-- image -->

<!-- image -->

# DINO KD as self-supervised learning

- For an image x, generate a set V of different views  construct different distorted views, or crops, of multicrop strategy
- two global views, xg1 and xg2  (2242 covering &gt; 50% of the image)
- several local views of smaller resolution (962 covering smaller areas, &lt; 50% of the image)
- All crops are passed through the student
- Only the global views are passed through the teacher
- This encourages learning “local-to-global” correspondences
- 
- 
- 
- Given a different crop of the  image, can the student predict what distribution the teacher produces?

<!-- image -->

# DINO Teacher and student networks

-  Teacher and student share the same architecture with different sets of parameters θs and θt 
-  NN g is composed of a backbone f (ViT or ResNet ), and of a projection head h:  g = h ◦ f. 
- The projection head consists of a 3-layer MLP with hidden size 2048, and a weight normalized fully connected layer
-  In downstream tasks use only the backbone f output. 
- Does not use BN in ViT
-  TEACHER
- Unlike classical KD, the teacher does not exists a priori and is build it from past iterations of the student network
- Use an exponential moving average (EMA) on the student weights: 
- 
- 

<!-- image -->

# DINO

<!-- image -->

The student must predict the teacher's distribution from a different crop. Same-view pairs are never used. 

-&gt; view-invariant, semantic representations

 

EMA teaches gt ← λ·gt + (1−λ)·gs λ goes from 0.996 tp 1 during training. 

Teacher is a stable moving target.

# DINO

-  COLLAPSE: trivial optimal solution is for both networks to output the same constant distribution 
1. Centering: subtract running mean c from teacher logits
- Prevents one dimension to dominate, and has a uniformization effect (B batch size)
- 
- 

1. Sharpening: low teacher temperature 
2. Opposite effect of sharpening
3. 

<!-- image -->

# DINO

<!-- image -->

<!-- image -->

x1, x2  - two different random views

With multi-crop (full DINO), the student also sees several small local crops. The teacher only ever sees the two global views.

Teacher saw view 1. Student saw at view 2. The student must predict the teacher's distribution from a completely different crop. Student to learn that both crops, despite looking different, belong to the same semantic concept.

<!-- image -->

Teacher starts as an exact copy of the student

<!-- image -->

Standard SGD step on the student only.

No gradient update of the teacher here

EMA update of the teacher. This is the only way the teacher can change

<!-- image -->

EMA update of the center C

cat([t1, t2]) stacks - teacher outputs from both views into shape (2n, K). .mean(dim=0) averages across all 2n samples -&gt; shape (K,)

C tracks the running mean of teacher outputs across the dataset

<!-- image -->

Converts student logits to a probability distribution. Higher temperature, softer distribution. Student is mor uncertain

<!-- image -->

1. Centering: subtract the running mean c. Removes whatever prototype the teacher always favors regardless of input (prevents mode collapse)

2. Sharpening: divide by teacher temperature much lower than student's 0.1 -&gt; sharper distribution.

forces the student to make confident predictions, which prevents uniform collapse

# DINO Results

<!-- image -->

<!-- image -->

# DINO  Properties

<!-- image -->

# DINO Properties

<!-- image -->

# DINO

-  DINO produces qualitatively different features -&gt;  the self-distillation objective encourages the [CLS] token to aggregate spatially and semantically coherent info from across the image. 
-  Use DINO when you need frozen features that work immediately
- DINO -&gt; the quality of its frozen representation
-  Use it for segmentation without finetuning, where patch-level attention maps trace object boundaries as an emergent property
- Use it for video object segmentation, where patch features are geometrically consistent enough across frames that temporal correspondence appears for free via nearest-neighbor matching.
-  DINO -&gt; overkill is full finetuning on classification
- 

# Masked Image Modelling

Masked Autoencoders Are Scalable Vision Learners

https://arxiv.org/abs/2111.06377 

# MAE

-  Success of masking in NLP (BERT)
-  What makes masked autoencoding different between vision and language?
- For a time, architectures were different: transformers vs CNNs
- Information density is different between language and vision.
- Language: highly semantic and information dense
- Images: exhibit high redundancy. A missing patch can be recovered from neighboring patches with little high-level understanding
- Decoder plays a different role
- Images: reconstructs pixels, its output is of a lower semantic level than common recognition tasks. 
- Language: it predicts missing words that contain rich semantic information.
- 
- 

# MAE

-  IDEA: mask random patches of the input image and reconstruct the missing pixels
-  Asymmetric encoder-decoder architecture
- encoder operates only on the visible subset of patches 
- a lightweight decoder that reconstructs the original image from the latent representation and mask tokens
-  Masking a high proportion of the input image, e.g., 75%, yields a meaningful self-supervisory task.

# MAE

<!-- image -->

# MAE Encoder

-  Divide an image into regular non-overlapping patches
-  Sample random patches without replacement following a uniform distribution
-  High masking ratio (i.e., the ratio of removed patches) eliminates redundancy
- 

# MAE ENCODER

- ViT applied only on visible, unmasked patches
- Only operates on a small subset 25% of the patches
- Mask patches are removed
- Can train large encoders, with only a fraction of compute and memory
- Uses positional embeddings

# MAE DECODER

-  Processes the full set of tokens consisting of 
- encoded visible patches
- mask tokens
-  Mask token: shared, learned vector that indicates the presence of a missing patch to be predicted
-  Add positional embeddings to all tokens in this full set to ensure mask tokens have no information about their location in the image
-  Only used during pre-training to perform the image reconstruction task
-  Independent of the encoder design
- 

# MAE Reconstruction target

-  MAE predicts the pixel values for each masked patch. 
- Each element in the decoder’s output is a vector of pixel values representing a patch. 
-  Last layer: a linear projection whose number of output channels equals the number of pixel values in a patch. 
- The decoder’s output is reshaped to form a reconstructed image. 
- Loss: MSE between the reconstructed and original images in the pixel space

# MAE Efficient implementation

1. Generate a token for every input patch (by linear projection with an added positional embedding)
2. Randomly shuffle the list of tokens and remove the last portion of the list, based on the masking ratio -&gt; small subset of tokens for the encoder (equivalent to sampling patches without replacement)
3. Encode tokens
4. Append mask tokens to the list of encoded patches
5. Unshuffle the full list (invert the random shuffle operation) to align all tokens with their targets
6. Decoder applied to this full list (with positional embeddings added).

No sparse operations are needed. 

# MAE Masking strategy

-  RANDOM: works best.
-  BLOCK: works reasonably well at a ratio of 50%, but degrades at a ratio of 75%. Harder task. Reconstruction is blurrier.
-  GRID: reconstruction is sharper. The representation quality is lower.

<!-- image -->

# MAE ImageNet experiments

<!-- image -->

# MAE Results on inputs with higher masking ratio

<!-- image -->

# MAE

<!-- image -->

# MAE

<!-- image -->

# MAE

-  MAE is a reconstruction method - it masks 75% of the image patches and trains the model to fill them back in 
- learns how images are structured  (textures, shapes, object parts) rather than learning to discriminate between images semantically
- MAE frozen features are weaker
- Do not use MAE when: you need frozen features, you need a plug-and-play backbone, or your task is pure classification without full finetuning
- Use MAE when you have a large ViT, you plan to finetune it fully on a downstream task, and that task requires dense spatial predictions (detection, segmentation, depth)