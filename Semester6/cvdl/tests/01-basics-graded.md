# Test 01 — Basics (graded, round 2)

Covers CNNs, semantic segmentation, transformers. Your second attempt, with corrections.
**Final score: 16 / 20 fully correct, 2 partial, 2 missed**

---

## Part A — CNNs (Lab 3)

1. The main building block of a CNN is the ______ layer, which slides small ______ across the input image and computes element-wise products with the region beneath.

   Answer: **convolutional, kernel(filters)** ✅ CORRECT

2. The three main hyperparameters of a convolutional layer (besides the number of filters) are: filter size F, padding ______, and ______ S.

   Answer: **P and stride** ✅ CORRECT

3. The Sobel filter is a ______-pass filter used to detect ______ in an image.

   Answer: **high, edges** ✅ CORRECT

4. ______ pooling keeps the largest value in each window, while ______ pooling keeps the mean of each window.

   Answer: **max, average** ✅ CORRECT (small typo: "avearage")

5. One full pass of the model through the entire training dataset is called an ______.

   Answer: **epoch** ✅ CORRECT

6. When using a pretrained model and training only the new final classifier (with the rest frozen), this is called ______ ______ (or linear probing).

   Answer: ~~feature selection~~ ❌ → **feature extraction**
   > Note: *feature selection* is a different ML concept (picking which input features to use). *Feature extraction* = freeze the backbone, train only a new classifier head on top.

7. In PyTorch, before each backward pass we must call `optimizer.______()` because gradients ______ by default.

   Answer: ~~set_zero, add up~~ ⚠️ partial → **zero_grad**, **accumulate**
   > Right idea; learn the exact method name `zero_grad()` and the exact word *accumulate*.

8. ______-CAM uses the gradient of the predicted class score with respect to a feature map to produce a class-specific ______.

   Answer: **GRAD, heatmap** ✅ CORRECT (Gradient-weighted Class Activation Mapping)

---

## Part B — Semantic Segmentation (Lab 4)

9. While image classification predicts one label per image, semantic segmentation predicts one label per ______.

   Answer: **pixel** ✅ CORRECT

10. The U-Net architecture has two paths: the ______ path (encoder) that captures context, and the ______ path (decoder) that restores resolution.

    Answer: ~~downscaling(shrinking), upscaling(expanding)~~ ⚠️ partial → textbook terms are **contracting**, **expanding**
    > Concept is right; the U-Net paper uses *contracting* and *expanding*. Use those in the exam.

11. ______ connections in U-Net pass feature maps directly from the encoder to the decoder, preserving high-resolution spatial detail.

    Answer: **skip** ✅ CORRECT

12. A learned upsampling layer in PyTorch is implemented with `nn.______`, also called transposed convolution or "deconvolution."

    Answer: **convTranspose2d** ✅ CORRECT (small spelling typo, correct is `ConvTranspose2d`)

13. The ______ loss is derived from a similarity coefficient that measures overlap between predicted and ground-truth masks, and is useful when the dataset is class-______ .

    Answer: ~~don't know~~ ❌ → **Dice**, **imbalanced**
    > Dice loss = 1 − Dice coefficient. The Dice coefficient measures **overlap** between predicted and ground-truth masks. Excellent when one class dominates (e.g. a tiny tumor in a huge background).

14. The standard segmentation metric, computed as the average IoU across all classes, is called ______ ______.

    Answer: **mean IoU** ✅ CORRECT (also written **mIoU**)

15. A `torch.utils.data.Dataset` answers "how do I get sample i?" while a `torch.utils.data.______` handles batching, shuffling, and parallel loading.

    Answer: **DataLoader** ✅ CORRECT

---

## Part C — Transformers (Lab 5)

16. Self-attention computes three vectors per token: the ______ (what I'm looking for), the ______ (what info I contain), and the ______ (the content I share).

    Answer: **query, key, value** ✅ CORRECT (Q, K, V)

17. The attention scores are computed as Q · K^T divided by √______ for numerical stability.

    Answer: **D (dimension of the embedding)** ✅ CORRECT

18. Because self-attention treats tokens as a set, transformers need ______ ______ to know where each token came from.

    Answer: **positional embeddings** ✅ CORRECT
    > Textbook term: *positional encodings*. *Embeddings* is fine in casual use; for the exam prefer *positional encodings*, especially when discussing the sinusoidal ones from "Attention is All You Need."

19. In a Vision Transformer, the image is first cut into small ______ , which are then projected into embeddings.

    Answer: **patches** ✅ CORRECT

20. The special learnable token added at the start of the sequence, used to aggregate global info for classification, is called the ______ token.

    Answer: **CLS** ✅ CORRECT (class token)

---

## Cheat sheet for the four things to lock in

1. **feature extraction** = freeze backbone + train head (not "feature selection").
2. **zero_grad()** + gradients **accumulate** = exact PyTorch wording.
3. U-Net paths: **contracting** (encoder) and **expanding** (decoder).
4. **Dice** loss for class-**imbalanced** segmentation; it measures **overlap**.
