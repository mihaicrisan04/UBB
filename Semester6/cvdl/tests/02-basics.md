# Test 02 — Basics (round 3) — GRADED

**Final score: 14 / 20 fully correct, 4 partial, 2 missed.**
Big wins on recycled questions (feature extraction, zero_grad/accumulate, contracting/expanding, Grad). Two stubborn gaps to fix: filter depth and the Dice loss.

---

## Part A — CNNs (Lab 3)

1. A CNN filter must have the same ______ as the input volume (so a filter applied to an RGB image is 3 channels deep).

   Answer: ~~(blank)~~ ❌ → **depth** (a filter applied to RGB has depth 3; depth = number of input channels)

2. The hyperparameter that controls how many pixels a filter jumps between applications is the ______.

   Answer: **stride S** ✅ CORRECT

3. Adding zero pixels around the border of an image so the filter can reach the edges is called ______.

   Answer: **padding P** ✅ CORRECT

4. A Gaussian filter is a ______-pass filter; a Sobel filter is a ______-pass filter.

   Answer: **low, high** ✅ CORRECT

5. The number of training samples that pass through the network before its weights are updated is called the ______ ______.

   Answer: **batch size** ✅ CORRECT

6. Adapting a pretrained model by freezing the backbone and training only a new classifier head is called ______ ______ (also known as linear probing).

   Answer: **feature extraction** ✅ CORRECT ⬆️ (was "feature selection" last test — locked in!)

7. In PyTorch, before computing the gradients for a batch we must call `optimizer.______()` because by default gradients ______ from previous backward passes.

   Answer: **zero_grad, accumulate** ✅ CORRECT ⬆️ (was "set_zero, add up" — locked in!)

8. To produce a class-specific heatmap of "where the model looked," we use ______-CAM, which weights feature maps by the ______ of the class score with respect to those feature maps.

   Answer: **Grad-CAM**, ~~(blank)~~ ⚠️ partial → second blank is **gradient** (the class score is weighted by its **gradient** with respect to the feature maps)

---

## Part B — Semantic Segmentation (Lab 4)

9. Semantic segmentation assigns one ______ to every ______ in the image.

   Answer: **label, pixel** ✅ CORRECT

10. U-Net's two paths are called the ______ path (encoder, captures context) and the ______ path (decoder, restores resolution).

    Answer: **contracting, expanding** ✅ CORRECT ⬆️ (was "downscaling/upscaling" — locked in!)

11. The connections that copy feature maps from the encoder to the corresponding decoder level (preserving spatial detail) are called ______ connections.

    Answer: **skip** ✅ CORRECT

12. The PyTorch class used for learned upsampling (transposed convolution) is `nn.______`.

    Answer: ~~convTrasnpose2d~~ ⚠️ partial → spelling: **`ConvTranspose2d`** (capital T, ends in `2d`). Concept is right, exam-graders are strict on case.

13. The loss derived from a similarity coefficient that measures the ______ between predicted and ground-truth masks is called ______ loss; it is especially useful for class-______ datasets.

    Answer: ~~difference, , ~~ ❌ → **overlap**, **Dice**, **imbalanced**
    > 🚨 Most important miss. Dice measures **overlap** (NOT difference — that's the opposite). It punishes "predict background everywhere" because it directly rewards intersection between predicted and ground-truth masks. Use it when classes are **imbalanced** (e.g. tiny tumor on huge background).

14. The most common multi-class segmentation metric, computed as the average IoU across all classes, is ______ ______ (abbreviated mIoU).

    Answer: **mean IoU** ✅ CORRECT

15. In PyTorch, the `Dataset` class defines how to load a single sample, while the ______ class defines how to iterate over them in batches with shuffling and parallel workers.

    Answer: **DataLoader** ✅ CORRECT

16. When applying a random crop or horizontal flip to a segmentation sample, the same geometric transformation must also be applied to the ______, or supervision breaks.

    Answer: **mask** ✅ CORRECT

---

## Part C — Transformers (Lab 5)

17. Self-attention produces three vectors per token: query, ______, and ______.

    Answer: **key, value** ✅ CORRECT

18. Attention scores are scaled by √D, where D is the ______ dimension, for numerical ______.

    Answer: **embedding**, ~~(blank)~~ ⚠️ partial → second blank is **stability** (scaling exists for *numerical stability* — keeps dot products from exploding as D grows)

19. Because self-attention is permutation-invariant, transformers add ______ ______ to the token embeddings so the model knows where each token came from.

    Answer: ~~positional embeddings~~ ⚠️ partial → textbook term is **positional encodings** (used interchangeably in some papers, but the lab + "Attention is All You Need" use *encodings*)

20. In a Vision Transformer, the image is split into small ______, each projected to an embedding; a special ______ token is prepended for classification.

    Answer: **patches, CLS** ✅ CORRECT

---

## Cheat sheet — the 3 things to lock in before round 4

1. **Filter depth** = number of input channels (RGB ⇒ depth 3).
2. **Dice loss** = `1 − Dice`. Dice measures **overlap** (not difference). Use for **imbalanced** classes.
3. Exact names: `ConvTranspose2d` (case-sensitive) and *positional **encodings*** (not embeddings).
