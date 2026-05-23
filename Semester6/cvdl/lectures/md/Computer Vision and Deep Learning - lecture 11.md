# COMPUTER VISION AND DEEP LEARNING

LECTURE 11

# Today’s agenda

-  Multimodal learning
-  Two-stream neural networks
-  CLIP
-  GLIP
- BLIP-2
- SAM

# Multimodal learning

-  Human perception is multimodal: when we understand a scene, we integrate vision, sounds, feelings
-  Multimodal learning: building models that can process, align, and reason over information from multiple modalities at once
-  Modality - distinct type of signal or data representation
- Most studied and this lecture’s focus: vision (images, videos) and language (text)
- Modalities can also include: audio, depth maps, thermal imagery, point clouds, physiological signals, etc.
- Challenge: learning meaningful relationships between modalities

# Unimodal vs. Multimodal

-  Unimodal models are limited
- Vision model (images)  - compress pixels it knows into a fixed set of class labels (e.g. dog), cannot along distinguish sematics like: "the dog is hiding on the left side of the sofa, partially occluded by a cushion“
- LLM (text) can describe a pet in high detail, but it hast’t “seen” one (doesn’t have any correspondence with actual pixels)
-  Multimodal learning
- Scale of supervision: manually annotating images is slow and expensive. Text generated continuously on the web along side images: product descriptions, news captions
- Expressiveness: label "dog" vs. phrase "a small brown dog jumping to catch a frisbee in mid-air“. Text: relations, attributes, negations, and comparisons
- Generalization: 1000 ImageNet categories vs. VLM can handle arbitrary text queries at test time (categories, descriptions, and concepts it was never explicitly trained on)

# Vision and Language – examples Visual Question Answering (VQA)

-  VQA: giving a correct natural language answer to a natural language question about a given image 
- Answering correctly requires detecting and categorizing multiple objects, counting them, performing a comparison, etc.
-  VQA
- Closed-ended setting: selects answers from a fixed vocabulary of candidate answers 
- Open-ended setting: model generates the answer token by token with no restriction on the output

<!-- image -->

# Vision and Language – examples Grounding

-  Grounding: connects abstract text  concepts to specific spatial locations in images or videos
-  identify, locate, and segment objects described by text (e.g., "the red car on the left")
- 

<!-- image -->

# Vision and Language – examples Referring Expression Comprehension (REC)

-  Referring Expression Comprehension
- input is a complete referring expression (a noun phrase constructed to uniquely identify one object in the scene) + the image
- output - a single bounding box for that object

# Vision and Language – examples Referring Expression Comprehension (REC)

<!-- image -->

# Vision and Language – examples Phrase grounding

-  Phrase grounding: model localizes multiple noun phrases from a sentence simultaneously, rather than a single expression in isolation.

<!-- image -->

# Two-Stream Convolutional Networks for Action Recognition in Videos

- Karen Simonyan AND Andrew Zisserman
- https://arxiv.org/pdf/1406.2199 

# Two-stream CNNs

-  Action recognition in videos: assigning a semantic label  (such as "playing tennis", "shaking hands", or "cutting vegetables“) to a video clip
-  One of the first architectures to tackle the problem of how to represent motion in video using deep networks
-  Videos:
- Spatial dimension: individual frame appearance, carries information about scenes and objects depicted in the video
- Temporal dimension: motion across the frames, conveys the movement of the observer (the camera) and the objects
- 
- 

# Optical flow

<!-- image -->

# Two-stream CNNs

<!-- image -->

Each stream is implemented using a deep ConvNet, softmax scores of which are combined by late fusion

Fusion:

1. averaging 
2. training a multi-class linear SVM on stacked L2-normalised softmax scores as features.

# Two-stream CNNs

-  Spatial stream
- operates on individual RGB frames (standard CNN)
- A single frame already carries a lot of information about what is happening
- 
- 

# Two-stream CNNs

-  Optical flow stream
- stack the flow channels dxy t of L consecutive frames to form a total of 2L input channels
- w,h -  width and height of the video
- 

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# Two-stream CNNs Multitask learning

-  Temporal ConvNet needs to be trained on video data, as video datasets were quite small: UCF-101 and HMDB-51 datasets, which have only: 9.5K and 3.7K videos respectively
-  Multitask learning: learn a (video) representation, which is applicable not only to the task in question (such as HMDB-51 classification), but also to other tasks (e.g. UCF-101 classification)
- two softmax classification layers on top of the last fully connected layer: one softmax layer computes HMDB-51 classification scores, the other one– the UCF-101 scores
- 

# Two-stream CNNs

<!-- image -->

# Two-stream CNNs

<!-- image -->

from each displacement field we subtract its mean vector

# Two-stream CNNs

-  temporal and spatial recognition streams are complementary, as their fusion significantly improves on both (6% over temporal and 14% over spatial nets)
-  SVM-based fusion of softmax scores outperforms fusion by averaging

<!-- image -->

# Contrastive learning multimodal architectures

# CLIP Learning Transferable Visual Models From Natural Language Supervision

- https://arxiv.org/abs/2103.00020 

# CLIP

- Learn a shared embedding space in which semantically related images and texts are geometrically close
- Zero-shot transfer: model can classify a sample even it it hasn’t seen a labeled example of the target classes during training.
- Retrieval: given a query in one modality, find the nearest neighbors in the other modality.
- Frozen encoder for VLMs.
- 

# CLIP

-  Uses a contrastive loss to unify text and images together
- During pre-training, align images with textual descriptor
- Trained on WIT (WebImageText) dataset
- 400M image-text pairs collected form a variety of publicly available sources on the Internet
- 

# CLIP

# CLIP

<!-- image -->

# CLIP

<!-- image -->

<!-- image -->

Jointly trains an image encoder and text encoder to maximize the cosine similarity of the image and text embeddings of the N real pairs in the batch while minimizing the cosinesimilarity of the embeddings of the N2−N incorrect pairings

# CLIP

<!-- image -->

Feature Extraction

Projection to shared embedding space

L2 normalization on the embedding dimension. All subsequent similarity computations reduce to cosine similarity (magnitude of the vectors carries no information, only their direction matters).

# CLIP

<!-- image -->

Similarity matrix: pairwise cosine similarities between every image and every text in the batch.

np.exp(t) is a learned temperature scaling factor. The raw temperature t is a scalar learned during training. Taking exp(t) ensures it stays positive.

# CLIP

<!-- image -->

Symmetric loss

The ground truth labels are simply [0, 1, 2, ..., n-1]. For image i, the correct text is at index i, and vice versa.

Pull matching image-text pairs together, push all non-matching pairs apart

# CLIP Training

- Models:
- 5 ResNets: ResNet-50, ResNet-101, and 3 EfficientNet-style scaled versions: RN50x4, RN50x16, RN50x64 (4x, 16x, 64x the compute of ResNet-50)
- 3 Vision Transformers: ViT-B/32, ViT-B/16, ViT-L/14
- All models trained for 32 epochs
- The largest ResNet model, RN50x64, took 18 days to train on 592 V100 GPUs while the largest Vision Transformer took 12 days on 256 V100 GPUs
-  Adam optimizer with decoupled weight decay regularization
-  Learning rate decayed using a cosine schedule
-  Very large minibatch size of 32,768 pairs
- 

<!-- image -->

ImageNet-R(endition) contains art, cartoons, deviantart etc.  of ImageNet classes.

ObjectNet is a large  test set for object recognition with control where object backgrounds, rotations, and imaging viewpoints are random.

ImageNet sketch: Google Image queries "sketch of \_\_", where \_\_ is the standard class name

ImageNet V2: new test data for the ImageNet benchmark

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# CLIP - limitations

-  Despite strong zero-shot performance, CLIP still underperforms task-specific supervised models on many fine-grained benchmarks
- Struggles with fine-grained classification tasks such as distinguishing car models, aircraft variants, or flower species
- Poor performance on abstract or systematic tasks like counting objects, recognizing spatial relationships, or reading text in images (OCR)
- Near-random performance on specialized datasets like MNIST; a model trained on 400 million image-text pairs struggles with handwritten digits it rarely saw described in natural language

# CLIP - limitations

-  Trained on 400 million image-text pairs scraped from the internet, which introduces significant social biases
- The internet-scraped data is noisy: many image-text pairs are only weakly aligned
- Extremely compute-hungry: training from scratch is inaccessible to most researchers
- Performance is sensitive to how prompts are worded: "a photo of a dog" and "dog" can give very different results, making deployment brittle without careful prompt engineering

# CLIP

-  Image-text retrieval: rank images by text query and vice versa
- Zero-shot classification: classify into any category without task-specific training
- Text-to-image generation: text encoder backbone in Stable Diffusion
- Vision-Language Models: vision backbone in LLaVA, InstructBLIP, and most modern VLMs
- Open-vocabulary detection &amp; segmentation: GLIP, Grounding DINO, CLIPSeg
- Medical imaging: domain-adapted variants: MedCLIP, BioViL, QuiltNet

# Grounded Language-Image Pre-training (GLIP)

- https://arxiv.org/pdf/2112.03857 

# GLIP What can it do?

-  Zero-shot detection: given a new dataset, just write the category names as a prompt - no fine-tuning needed
- GLIP-L scores 49.8 AP on COCO without seeing a single COCO image during training
- Few-shot transfer: adapts to new domains with very little data
- Open-vocabulary detection: not limited to a fixed set of categories: any noun phrase in the prompt is a valid query
- Can localize rare, attribute-rich, or even abstract concepts ("beautiful caribbean sea turquoise", "the view")
- Prompt tuning: instead of fine-tuning the whole model per task, only tune a small prompt embedding

# Grounded Language-Image Pre-training (GLIP)

-  Phrase grounding: identifying the fine-grained correspondence between phrases in a sentence and objects (or regions) in an image
-  GLIP reformulates object detection as phrase grounding
-  Model takes as input:
- Image
- Text prompt that describes all the objects in that image (e.g. COCO: 80 phrases, i.e., the 80 COCO object class names, joined by “. ” Person.Bicycle. … Hairdryer)
-  Replaces the object classification logits in the bbox classifier with the word-region alignment scores
- 
- 

# GLIP

<!-- image -->

# GLIP Unified Formulation

-  Classical object detection: classify each region into one of c predefined categories
-  GLIP reframes this: instead of predicting a class label, align each region to a phrase in a text prompt
-  Object detection model
- A visual encoder extracts region features O from an image
- Each region/box feature is fed into two prediction heads
- box classifier C
- box regressor R
- Two stage proposals also use a region proposal loss, but this is integrated in the localization loss (it does not use semantic info of object classes)

<!-- image -->

# GLIP Unified Formulation

-  Box classifier
- 
- 
- O ∈ R N×d are the object/region/box features of the input image
- W ∈ R c×d is the weight matrix of the box classifier C
- S cls ∈ R N×c are the output classification logits
- T ∈ {0, 1} N×c is the target matching between regions and classes computed from the classical many-to-1 matching or the bipartite Hungarian match 
- loss(S; T) CE for two-stage detectors and a focal  loss for one-stage detectors
- 
- 
- 

<!-- image -->

# GLIP Unified Formulation

-  Reformulate detection as a grounding task, by grounding/aligning each region to c phrases in a text prompt 
-  How to design a text prompt for a detection task? 	
- Prompt = “Detect: person, bicycle, car, ... , toothbrush” . Each class name is a candidate phrase to be grounded
- Compute the alignment scores Sground between image regions and words in the prompt
- 
- P ∈ RM×d is the contextual word/token features from the language encoder (similar role to the weight matrix W in classification loss)
-  The grounding model (the image encoder EncI +  the language encoder EncL) is trained end-to-end by minimizing the loss:
- 
- 

- 

<!-- image -->

<!-- image -->

<!-- image -->

<!-- image -->

# GLIP Unified Formulation

-  Problem
- Sground ∈ R N×M and the target T ∈ {0, 1} N×c  
-  The number of (sub)- word tokens M is always larger than the number of phrases c in the text prompt:
- some phrases contain multiple words, e.g., “traffic light”
- some single word phrases are splitted into multiple (sub)-word tokens, e.g., “toothbrush” to “tooth#” and “#brush”
- some are the added tokens, such as “Detect:”, “,”, special tokens in language models
- [NoObj] token is added at the end of the tokenized sequence
-  Classification loss binary sigmoid loss: target matrix T ∈ {0, 1} N×c is expanded to T’ ∈ {0, 1} N×M by making all sub-words positive match if a phrase is a positive match and all added tokens negative match to all image features
- During inference, average token probabilities as the phrase probability
- 

# GLIP Language-Aware Deep Fusion

-  In the basic formulation, image and text are encoded independently: only fused at the final dot product -&gt;LATE FUSION
-  VL research shows that deep fusion is necessary for high-quality grounding 
-  Late fusion cannot capture fine-grained cross-modal interactions during feature extraction
-  Fusion is introduced in the last few layers of both encoders
- At each fusion layer i, cross-modal multi-head attention (X-MHA) is applied: 
- Image features attend to language features -&gt; language-aware visual features
- Language features attend to image features -&gt; vision-aware text features
- 
- 

# GLIP Language-Aware Deep Fusion

-  Fusion
- 
- 
- 
- L is the number of DyHeadModules in DyHead
- BERTLayer is newly-added BERT Layers on top of the pretrained BERT
- O0 denote the visual features from the vision backbone, and P0 denote the token features from the language backbone (BERT)
- 
- 
- 
- 
- 

<!-- image -->

# GLIP Language-Aware Deep Fusion

-  Cross attention
- 
- 
- 
- 
-  {W(symbol,I) , W(symbol,L) : symbol ∈ {q, v, out}} are trainable parameters and play similar roles to those of query, value, and output linear layers in Multi-Head Self Attention
- 
- 
- 

- 
- 
- 
- 

<!-- image -->

# GLIP Language-Aware Deep Fusion

<!-- image -->

project image features to query space 

project text features to key space 

compute attention scores (image queries vs text keys) 

# GLIP Language-Aware Deep Fusion

<!-- image -->

project text features to value space 

project image features to value space 

# GLIP Language-Aware Deep Fusion

<!-- image -->

image regions attend to text values 

-&gt; text-informed image 

text tokens attend to image values (via transposed attn) 

-&gt; image-informed text update

# GLIP

<!-- image -->

1. X-MHA - produces cross-modal updates for both sides 
2. 
3. Oi + cross-modal update -&gt; DyHeadModule -&gt; Oi+1

 

3.  Pi + cross-modal update -&gt; BERTLayer → Pi+1

# GLIP Language-Aware Deep Fusion

-  The visual encoder learns to produce language-aware region features: the same region is represented differently depending on the text prompt 
-  The language encoder becomes aware of image content, producing context-sensitive phrase features 
-  Model is much stronger at grounding rare, abstract, or attribute-rich phrases

# GLIP Overview

<!-- image -->

Alignment score matrix

N region features O₁, O₂, … ON (one per detected box) 

M token features P₁, P₂, … PM (one per text token)

# GLIP Overview

<!-- image -->

How well does region i match token j?

# GLIP Pre-training Data

-  Detection data - standard annotated datasets (COCO, Objects365, etc.) with bounding boxes and fixed category labels; reformulated as grounding via text prompts 
-  Grounding data - must cover a much larger vocabulary of visual concepts than existing detection data 
- VG Caption: 110,689 unique phrases
- Flickr30k Entities: 44,518 unique phrases 
-  Image-text pairs - 24M web-crawled pairs with no explicit box annotations -&gt; handled via self-training

# GLIP Pre-training Data

-  Detection data 
- Grounding data 
- Image-text pairs 
- Teacher: GLIP trained on 3M human-annotated grounding data
- Parse: extract noun phrases from 24M web captions using an NLP parser
- Label: teacher predicts bounding boxes for each noun phrase -&gt; keep only high-confidence ones (&gt;0.5) -&gt; 78.1M pseudo phrase-box pairs
- Student: train larger GLIP-L on all 27M examples (3M human + 24M pseudo-labeled)
- 

<!-- image -->

# GLIP Results

<!-- image -->

# GLIP Object Detection in the Wild 

-  13 downstream detection datasets:
- Fine-grained species detection
- Drone-view detection
- Ego-centric detection
- Various rare and domain-specific categories

<!-- image -->

# GLIP Prompt tuning – manual prompt engineering 

-  GLIP's output is conditioned on the text prompt, so a better prompt = better detection 
- Simply add descriptive attributes to the prompt to inject domain knowledge 
- Example: "stingray" alone fails -&gt; "flat and round stingray" works 
- AP50 on stingray: 4.6 -&gt; 9.7 with just a prompt change
- No annotated data, no retraining

<!-- image -->

# GLIP Prompt tuning

-  Each task has a  fixed prompt (e.g. "Detect pothole.") 
- Get initial prompt embeddings P⁰ from the language backbone, then discard the backbone
-  Only fine-tune P⁰ as a learned task-specific input: all grounding model parameters stay frozen
-  One model, all tasks, tiny tuning cost

<!-- image -->

# Interactive segmentation: Segment anything (SAM)

https://arxiv.org/abs/2304.02643 

 

# SAM

-  Foundation model for image segmentation: a promptable model and pre-trained it on a broad dataset using a task that enables powerful generalization. 
- Three aspects: task, model and data
- What task will enable zero-shot generalization?
- What is the corresponding model architecture?
- What data can power this task and model?
- 

# SAM Task

-  Promptable segmentation task: the goal is to return a valid segmentation mask given any segmentation prompt
- Prompt: points, bboxes, text
- Valid mask: even for ambigous prompts the output should be a reasonable mask for at least one of those objects
- 

<!-- image -->

# SAM Model

-  Requirements:
- flexible prompts
- compute masks in amortized (interactive use)
- must be ambiguity-aware
- Solution
- Independent image and prompt encoders
- lightweight mask decoder that predicts segmentation masks
- Inference: ~ 50 ms in a web browser
- 

<!-- image -->

# SAM Dataset

-  build a “data engine” with a model-in-the-loop dataset annotation
- SA-1B dataset: 1B masks from 11 M images
- 400× more masks than any existing segmentation dataset; high quality and diverse masks
- 

<!-- image -->

# SAM SA-1B dataset annotation

-  3 stage annotation
- Assisted Manual: Annotators annotate alongside SAM to pick all masks in an image.
- Semi-Automatic: Annotators are asked to only annotate masks for which SAM cannot render a confident prediction.
- Full-Auto: SAM is allowed to fully predict masks given its ability to sort out ambiguous masks via a full sweep.
- a regular grid of foreground points, yielding on average ∼100 high-quality masks per image
- 

# SAM SA-1B dataset diversity

<!-- image -->

# SAM Architecture

<!-- image -->

MAE pre-trained Vision Transformer (ViT) 

runs once per image and can be applied prior to prompting the model

# SAM Architecture

<!-- image -->

MAE pre-trained Vision Transformer (ViT) 

runs once per image and can be applied prior to prompting the model

<!-- image -->

MAE

<!-- image -->

# SAM Architecture

- Prompt encoder:
- Points: 
- positional encoding for (x, y) position+ trained embedding for fg/bg
- BBoxes
- Positional encoding + embedding fw/bg for top left point
- Positional encoding + embedding fw/bg for bottom right point
- Text: CLIP text embeddings
- 

<!-- image -->

# SAM Architecture

- Prompt encoder:
- Points: 
- positional encoding for (x, y) position+ trained embedding for fg/bg
- BBoxes
- Positional encoding + embedding fw/bg for top left point
- Positional encoding + embedding fw/bg for bottom right point
- Text: CLIP text embeddings
- 

<!-- image -->

# SAM Architecture

- Mask decoder: maps the image embedding and a set of prompt embeddings to an output mask
- Mask decoder uses modification of a Transformer decoder block followed by a dynamic mask prediction head.
- The block uses prompt self-attention and cross-attention in two directions (prompt-to-image embedding and vice-versa)
- 
- 

# SAM Mask decoder

<!-- image -->

insert into the set of prompt embeddings a learned output token embedding that will be used at the decoder’s output, analogous to the [CLS]

# SAM Mask decoder

-  Input: [mask token 1] [mask token 2] [mask token 3] + prompt tokens + image embedding
- The 3 output tokens interacted and have information from the image and the prompt
-  Each token is dot-producted with the image embedding to produce a spatial mask 
- Mask Token 1 -&gt; attends broadly -&gt; whole object mask
- Mask Token 2 -&gt; attends to parts -&gt; part-level mask
- Mask Token 3 -&gt; attends finely -&gt; subpart mask

# SAM Mask decoder

- Transformer decoder
- (1) SA on the tokens
- (2) cross-attention from tokens (as queries) to the image embedding
- (3) MLP updates each token
- (4) cross-attention from the image embedding (as queries) to tokens
- 
- 

<!-- image -->

# SAM Mask decoder

- To ensure the decoder has access to geometric information the positional encodings are added to the image embedding whenever they participate in an attention layer
- The entire original prompt tokens (including their positional encodings) are re-added to the updated tokens whenever they participate in an attention layer
- 
- 

# SAM Mask decoder

<!-- image -->

Prompt aware image embeddings

- Upsampled by 4× with two transposed convolutional layers 

<!-- image -->

# SAM Mask decoder

Image aware prompt embeddings

- the tokens attend once more to the image embedding
- passed to a small 3-layer MLP that outputs a vector matching the channel dimension of the upscaled image embedding

<!-- image -->

# SAM Mask decoder

Image aware prompt embeddings

- the tokens attend once more to the image embedding
- passed to a small 3-layer MLP that outputs a vector matching the channel dimension of the upscaled image embedding

# SAM Ambiguities

-  With one output, the model will average multiple valid masks (ambiguous prompt)
- Model predicts 3 output masks
- Whole
- Part
- Subpart

<!-- image -->

<!-- image -->

# SAM

# SAM Interactive training

-  SAM needs to learn to respond to user prompts: during training you need to simulate prompts. 
- Point prompt simulation: close to the shape center (in practice, humans click somewhere roughly in the middle of the object)
- Bounding box prompt simulation: Take the tight bounding box around the ground truth mask, then add random jitter
- 

<!-- image -->

https://www.youtube.com/watch?v=OhxJkqD1vuE 

# SAM Interactive training

https://www.youtube.com/watch?v=OhxJkqD1vuE 

<!-- image -->

# SAM Interactive training

-  A single point prompt is ambiguous: it could refer to the whole object, a part, or a subpart
- SAM predicts 3 masks simultaneously in one forward pass, one per granularity level
- Each mask gets a predicted IoU score - SAM's self-assessed confidence of mask quality
- The mask with the highest IoU score is automatically selected
- The whole/part/subpart hierarchy is not explicitly labeled
- It emerges naturally during training
- A point often falls inside multiple nested ground truth masks
- Each output token learns to match the best fitting mask at its granularity level

https://www.youtube.com/watch?v=OhxJkqD1vuE 

# SAM Interactive training

https://www.youtube.com/watch?v=OhxJkqD1vuE 

<!-- image -->

<!-- image -->

# SAM Interactive training

https://www.youtube.com/watch?v=OhxJkqD1vuE 

Sample a point from the error region, and input it into the network as a new  point prompt

# SAM Interactive training

https://www.youtube.com/watch?v=OhxJkqD1vuE 

<!-- image -->

<!-- image -->

# SAM Interactive training – full training loop

- Give SAM a random prompt -&gt; predict mask
- Compute error region vs ground truth
- Sample next corrective point from error region
- Feed corrected prompt back to SAM -&gt; predict new mask
- Repeat up to N times
- 
- SAM is trained to handle multi-click interactive segmentation, getting progressively better with each correction: exactly how a real human annotator would use it.
- 

# SAM Results

<!-- image -->

<!-- image -->

# SAM Results

<!-- image -->

<!-- image -->

# BLIP-2 Bootstrapping Language-Image Pre-training with Frozen Image Encoders and Large Language Models

https://arxiv.org/pdf/2301.12597 

# BLIP-2 What can it do?

<!-- image -->

Visual knowledge reasoning : "tell me the history of this place" 

Personalized captions: describe specific people/objects in the image

# BLIP-2 What can it do?

<!-- image -->

Visual commonsense reasoning: "is this photo unusual?" 

# BLIP-2 What can it do?

<!-- image -->

Visual question answering: multi-turn conversation about an image 

# BLIP-2 What can it do?

<!-- image -->

Storytelling: "write a romantic message for this photo"

# BLIP-2

-  BLIP-2 -  vision-language pre-training method that bootstraps from frozen pre-trained unimodal models
-  Other models train everything end-to-end: image encoder + language model together
- Powerful frozen models already exist: e.g. CLIP for vision, GPT/T5 for language
- why retrain what already exists?
- Train only a lightweight Querying Transformer (Q-Former) that bridges the two

# Q-Former

Q-Former- bridge between two modalities (image and text)

- Two transformer modules that share the same weights:
- image transformer that interacts with the frozen image encoder for visual feature extraction; gets as input a set of learnable query embeddings
- text transformer that can function as both a text encoder and a text decoder

<!-- image -->

- Q-Former pre-trained in two stages: 
- (1) vision-language representation learning stage with a frozen image encoder 
- (2) vision-to-language generative learning stage with a frozen LLM

<!-- image -->

# Q-Former

- Q-Former consists of two transformer submodules sharing the same self-attention layers:
- Image transformer: interacts with the frozen image encoder via cross-attention
- Text transformer: functions as both text encoder and text decoder
- Initialized from BERTbase weights; cross-attention layers randomly initialized
- 

# Q-Former

<!-- image -->

A fixed set of 32 learnable query embeddings (each 768-dim) are the input to the image transformer 

Queries interact with image features via cross-attention every other block 

Output: Z ∈ R(32×768)  - a compact fixed-size visual representation regardless of image resolution

Queries interact with text via shared self-attention layers

# Q-Former

-  The frozen image encoder produces large feature maps (e.g. 257×1024 for ViT-L/14) 
- Q-Former compresses this into just 32×768 
- The bottleneck forces the queries to extract only the visual information most relevant to language 
-  LLM not overwhelmed with raw visual features it was never trained on
- Reduces catastrophic forgetting risk -&gt; LLM never needs to adapt

# Q-Former Stage 1 - Vision-Language Representation Learning

-  Q-Former is connected to the frozen image encoder and trained with 3 objectives, each using a different attention mask:
- Image-Text Matching (ITM) - queries and text can attend to each other (bidirectional mask); binary classification: does this image match this text?
- Image-Grounded Text Generation (ITG) - queries see each other but not the text; text tokens can attend to all queries and previous tokens (causal mask); forces queries to extract everything needed to generate the caption
- Image-Text Contrastive Learning (ITC) - queries and text cannot see each other (unimodal mask); maximize similarity of matched pairs, minimize for non-matched

<!-- image -->

# Q-Former Stage 2 - Vision-to-Language Generative Learning

-  A single fully-connected layer projects Q-Former output Z from 768-dim to the LLM's input dimension
- The 32 projected query embeddings are prepended to the text embeddings as soft visual prompts
- The LLM sees them as if they were part of its normal text input: no architectural changes to the LLM
- Two LLM types supported:
- Decoder-only (e.g. OPT): language modeling loss: generate text conditioned on visual prompts
- Encoder-decoder (e.g. FlanT5): prefix language modeling - visual prompts + prefix text -&gt; encoder; suffix text -&gt; decoder generates

# Q-Former Stage 2 - Vision-to-Language Generative Learning

<!-- image -->

# Qformer Training datasets

-  129M images total: COCO, Visual Genome, CC3M, CC12M, SBU, LAION400M
-  Captions enriched using CapFilt - synthetic captions generated and ranked by CLIP similarity, top-2 kept per image
- Image encoders: ViT-L/14 (CLIP) or ViT-g/14 (EVA-CLIP), second-to-last layer features used
- LLMs: OPT family (decoder) or FlanT5 family (encoder-decoder)
- Training cost: largest model (ViT-g + FlanT5-XXL) trains in &lt;6 days on 16 A100s