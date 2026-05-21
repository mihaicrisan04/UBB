# Test 03 — Basics (round 4) — GRADED

**Final score: 13 / 20 fully correct, 5 partial, 2 missed.**
Big wins: **depth** (Q1), `ConvTranspose2d` (Q12), **overlap** (Q13), and *encodings* (Q19) all locked in from previous tests.
Still to fix: pooling = **no** params, transfer learning vs fine-tuning, the loss name **Dice**, and `argmax`.

---

## Part A — CNNs (Lab 3)

1. A convolutional filter must have the same ______ as the input volume — for an RGB image, the filter has 3 channels.

   Answer: **depth** ✅ CORRECT ⬆️ (was blank last test — locked in!)

2. Given input width `W_in`, filter size `F`, padding `P`, and stride `S`, the output width is `W_out = (W_in − F + 2P) / ______ + 1`.

   Answer: **S** ✅ CORRECT

3. The ______ activation function, applied after most convolutional layers, replaces negative values with zero.

   Answer: **ReLU** ✅ CORRECT (Rectified Linear Unit)

4. Pooling layers have ______ learnable parameters.

   Answer: ~~automatic~~ ❌ → **no** (or **zero / 0**) — pooling is a fixed operation (max or mean over a window), nothing is learned. That's *why* it's just a cheap way to shrink feature maps.

5. Reusing a model pretrained on ImageNet as a starting point for a new task is called ______ ______.

   Answer: ~~fine-tuning~~ ⚠️ partial → **transfer learning**
   > Subtle but important: *Transfer learning* is the umbrella concept (reuse a pretrained model). *Fine-tuning* is one specific strategy under transfer learning (unfreeze the whole model and keep training). *Feature extraction / linear probing* is the other strategy (freeze backbone, train only the head).

6. Adapting a pretrained model by freezing the backbone and training only a new classifier head is called ______ ______ (or linear probing).

   Answer: **feature extraction** ✅ CORRECT

7. To prevent gradients from accumulating across batches, PyTorch users call `optimizer.______()` before each backward pass.

   Answer: **zero_grad** ✅ CORRECT

8. ______-CAM produces a class-specific heatmap by weighting feature maps with the ______ of the predicted class score with respect to those feature maps.

   Answer: **Grad-CAM**, ~~(blank)~~ ⚠️ partial → second blank is **gradient** (Grad-CAM = **Grad**ient-weighted CAM; you weight feature maps by the *gradient* of the score)

---

## Part B — Semantic Segmentation (Lab 4)

9. The output of a segmentation model has depth equal to the number of ______, where each channel is a probability map for one class.

   Answer: ~~channels~~ ❌ → **classes**
   > Tricky one. *Depth* and *channels* are the same thing in a tensor, so "depth = number of channels" is tautological. The point of the sentence is *what determines that number* — it's the number of classes (1 channel per class). Re-read: "depth equal to the number of **classes**, where each channel is a probability map for one class." That makes sense.

10. U-Net's two paths are the ______ path (encoder) and the ______ path (decoder).

    Answer: **contracting, expanding** ✅ CORRECT

11. The connections that copy encoder feature maps directly to the decoder at the same level are called ______ connections.

    Answer: **skip** ✅ CORRECT

12. The PyTorch class for learned upsampling (transposed convolution) is `nn.______` — note the exact spelling, capitalization matters.

    Answer: **ConvTranspose2d** ✅ CORRECT ⬆️ (was misspelled twice before — locked in!)

13. The Dice coefficient measures the ______ between the predicted mask and the ground-truth mask. The corresponding loss, called ______ loss, is especially useful when classes are ______ .

    Answer: **overlap** ✅, ~~entropy~~ ❌, ~~cross-entropy~~ ❌ → **Dice**, **imbalanced**
    > 🚨 Half-win: you got **overlap** right this time! That was your biggest gap. Now lock in the rest:
    > - The loss is just called **Dice** loss (after the Dice coefficient itself).
    > - It's the go-to choice when classes are **imbalanced** (one class dominates, like a tiny tumor in a huge background).
    > - Cross-entropy is a *different* loss — it's the standard pixel-wise classification loss; it's *not* the answer here.

14. To turn the network's per-pixel probability output into a final segmentation mask, you apply ______ over the class dimension.

    Answer: ~~(blank)~~ ❌ → **argmax** (pick the index of the most-likely class at each pixel)

15. The average IoU across all classes is the ______ ______ (abbreviated mIoU).

    Answer: **mean IoU** ✅ CORRECT

16. In a PyTorch data pipeline, `Dataset` defines a single sample, while ______ defines how to iterate over them in batches.

    Answer: **DataLoader** ✅ CORRECT

17. When applying a random rotation as a data augmentation in segmentation, the rotation must also be applied to the ______, not just the image.

    Answer: **mask** ✅ CORRECT

---

## Part C — Transformers (Lab 5)

18. Self-attention produces three vectors per token: query, key, and ______. The attention scores are scaled by √______ for numerical ______.

    Answer: **value**, **D**, ~~"so that they dont get too big when D get too large"~~ ⚠️ partial → the word is **stability**
    > Your explanation is correct — that's literally *what* numerical stability means. The exam word is **stability** (or *numerical stability*).

19. To inject spatial structure (so the model knows where each token came from), transformers add ______ ______ to the token embeddings — the lab uses the term *encodings*, not *embeddings*.

    Answer: **positional encodings** ✅ CORRECT ⬆️ (typo "econdings" — but you finally used *encodings*, not *embeddings*. Locked in!)

20. In a Vision Transformer, the image is first cut into small ______ ; a special ______ token is then prepended to the sequence and used for the final classification.

    Answer: **patches, CLS** ✅ CORRECT

---

## Progress tracker

| Concept | Test 01 | Test 02 | Test 03 |
| --- | --- | --- | --- |
| feature extraction | ❌ | ✅ | ✅ |
| zero_grad + accumulate | ❌ | ✅ | ✅ |
| Grad-CAM | ❌ | ⚠️ (Grad only) | ⚠️ (Grad only) |
| contracting/expanding | ⚠️ | ✅ | ✅ |
| filter depth | — | ❌ | ✅ |
| ConvTranspose2d (exact) | ❌ | ⚠️ typo | ✅ |
| Dice = overlap | ❌ | ❌ | ✅ |
| Dice loss + imbalanced | ❌ | ❌ | ❌ |
| positional encodings | ⚠️ | ⚠️ | ✅ |

## Cheat sheet — 4 things to lock in before round 5

1. Pooling has **no** (zero) learnable parameters.
2. **Transfer learning** = the umbrella concept. *Fine-tuning* and *feature extraction* are two strategies *under* it.
3. The loss derived from the Dice coefficient is called **Dice** loss. It's used when classes are **imbalanced**. (Cross-entropy is a *different* loss; don't mix them up.)
4. To convert per-pixel probabilities into a final segmentation mask, apply **argmax** over the class dimension.
5. Bonus: in Grad-CAM, the feature maps are weighted by the **gradient** of the class score.
