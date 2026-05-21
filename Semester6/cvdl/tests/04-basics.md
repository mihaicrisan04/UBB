# Test 04 — Basics (round 5) — GRADED

**Final score: 14 / 20 fully correct, 3 partial, 3 missed.**

🎉 Major wins — three of the most stubborn concepts FINALLY locked in:
- **Dice loss + imbalanced** (Q9) — first time fully correct after missing it 3 times!
- **argmax** for final mask (Q11)
- numerical **stability** (Q15)

⚠️ Remaining issues:
- **gradient** (Q4) — left blank for the **third test in a row**. This is the one to memorize.
- **classes vs channels** (Q14) — fell into the same trap as Test 03 Q9.
- **backbone / head** (Q5) — left blank; this is on the study guide.

---

## Part A — CNNs (Lab 3)

1. Pooling layers have ______ learnable parameters; they exist to reduce the ______ size of feature maps.

   Answer: **no, spatial** ✅ CORRECT ⬆️ (was "automatic" before — locked in!)

2. *Transfer learning* is the umbrella concept of reusing a pretrained model. Its two main strategies are ______ ______ (freeze the backbone, train only a new head) and full ______-______ (unfreeze everything and keep training).

   Answer: **feature extraction and fine-tuning** ✅ CORRECT ⬆️ (small typo "fine-tunning"; hierarchy locked in!)

3. The activation function known as ______ (full name: Rectified Linear Unit) replaces negative values with ______ .

   Answer: **ReLU, zeros** ✅ CORRECT

4. Grad-CAM produces a class-specific heatmap by weighting each feature map by the ______ of the predicted class score with respect to that feature map.

   Answer: ~~(blank)~~ ❌ → **gradient**
   > 🚨 Third test in a row this blank is empty. Memory hook: **Grad**-CAM = **Grad**ient-weighted CAM. The word *gradient* is literally in the name.

5. In a pretrained model, the large feature-extracting body is called the ______, and the small task-specific final layer (e.g. the classifier) is called the ______ .

   Answer: ~~(blank)~~ ❌ → **backbone, head**
   > Standard vocab. The "backbone" is the pretrained body (e.g. ResNet18 minus its final layer); the "head" is the small replaceable classifier on top.

6. A model that performs very well on the training data but poorly on unseen data is said to ______ .

   Answer: ~~overfitt~~ ⚠️ partial → **overfit** (one t) or **overfitting** as a noun. Concept right.

7. During inference/evaluation we usually wrap forward passes in `torch.______()` to skip gradient tracking, and we switch the model to ______ mode so layers like dropout and batch norm behave correctly.

   Answer: **no_grad, eval** ✅ CORRECT

8. A common CNN building block is: convolution → activation (like ReLU) → ______ (to reduce spatial size and grow the receptive field).

   Answer: **pooling** ✅ CORRECT

---

## Part B — Semantic Segmentation (Lab 4)

9. The loss derived from the Dice coefficient is called ______ loss. It is particularly useful for class-______ datasets (where one class dominates).

   Answer: **Dice loss, imbalanced** ✅ CORRECT 🎉 ⬆️ (missed 3 times in a row before this — fully locked in!)

10. The standard pixel-wise classification loss used in segmentation (applied independently at each pixel) is ______-______ loss. (This is a *different* loss from Dice.)

    Answer: **cross-entropy** ✅ CORRECT

11. To convert the network's per-pixel probability output into a final segmentation mask, you apply ______ over the class dimension.

    Answer: **argmax** ✅ CORRECT 🎉 ⬆️ (was blank in Test 03 — locked in!)

12. Skip connections in U-Net help preserve ______-resolution spatial detail that the encoder's pooling layers would otherwise destroy.

    Answer: **high** ✅ CORRECT

13. In practice we often combine the two segmentation losses as `L = L_CE + λ · L_______`, taking the stability of cross-entropy and the overlap focus of the other.

    Answer: **Dice** ✅ CORRECT

14. The output of a U-Net segmentation model is a volume whose depth equals the number of ______ in the problem (one channel per class).

    Answer: ~~channels~~ ❌ → **classes**
    > Same trap as Test 03 Q9. Re-read: "depth equal to the number of ______ in the problem (one channel per class)." If you fill in "channels", it becomes circular ("depth = number of channels"). The point is *what determines that number* — it's the number of **classes** (1 channel per class).

---

## Part C — Transformers (Lab 5)

15. In self-attention, dividing `Q · K^T` by √D (where D is the embedding dimension) is done for numerical ______.

    Answer: **stability** ✅ CORRECT 🎉 ⬆️ (you described the concept correctly last test but couldn't recall the word — now locked in!)

16. Self-attention treats tokens as a ______ (so swapping two tokens gives the same output). To fix this, transformers add positional ______ to the token embeddings.

    Answer: ~~message passing~~, **encodings** ⚠️ partial → first blank is **set**
    > Tokens are treated as a **set** — an unordered collection. That's why swapping them produces the same output (sets have no order). "Message passing" is Karpathy's intuition for *what attention does*, but here the blank wants a noun describing how tokens are seen by attention.

17. The CLS token's role is to ______ information from all patch tokens into a single vector that is used for the final classification.

    Answer: **gather** ✅ accepted (textbook word: **aggregate** — same meaning)

18. In multi-head attention, the model runs several Q/K/V projections in ______ and then ______ their outputs into one vector.

    Answer: **parallel, concatenate** ✅ CORRECT

19. In a Vision Transformer, the image is split into small ______, each of which is linearly projected into an ______ vector.

    Answer: **patches**, ~~(blank)~~ ⚠️ partial → second blank is **embedding** (each patch is linearly projected into an **embedding** vector)

20. Karpathy's intuition for self-attention: it behaves like message passing on a fully ______ graph, where every token is a ______ that exchanges info with every other one.

    Answer: **connected, node** ✅ CORRECT

---

## Progress tracker (across all 4 tests)

| Concept | T01 r1 | T01 r2 | T02 | T03 | T04 |
|---|---|---|---|---|---|
| feature extraction | ❌ | ❌ | ✅ | ✅ | ✅ |
| zero_grad + accumulate | ❌ | ❌ | ✅ | ✅ | ✅ |
| contracting / expanding | — | ⚠️ | ✅ | ✅ | — |
| skip connections | — | ❌ | ✅ | ✅ | ✅ |
| ConvTranspose2d (exact) | — | ❌ | ⚠️ | ✅ | — |
| filter depth | — | — | ❌ | ✅ | — |
| Dice = overlap | — | — | ❌ | ✅ | — |
| **Dice loss + imbalanced** | — | — | ❌ | ❌ | ✅ 🎉 |
| positional encodings | — | ⚠️ | ⚠️ | ✅ | ✅ |
| mean IoU | — | ❌ | ✅ | ✅ | — |
| DataLoader | — | ❌ | ✅ | ✅ | — |
| CLS token | — | ❌ | ✅ | ✅ | ✅ |
| pooling = no params | — | — | — | ❌ | ✅ 🎉 |
| argmax for final mask | — | — | — | ❌ | ✅ 🎉 |
| numerical stability (word) | — | — | — | ⚠️ | ✅ 🎉 |
| transfer learning hierarchy | — | — | — | ⚠️ | ✅ |
| **Grad-CAM = gradient** | — | ❌ | ❌ | ❌ | ❌ |
| classes vs channels (depth) | — | — | — | ❌ | ❌ |

Three big wins this round (🎉). One genuine plateau: **Grad-CAM's second blank** has been missed in every test. The fix is a 30-second re-read of the Grad-CAM section in the study guide — the word *gradient* is literally in the algorithm steps.

## Cheat sheet — 3 things before round 6

1. **Grad-CAM weights feature maps by the GRADIENT of the class score.** (Grad-CAM = **Grad**ient-weighted CAM.)
2. U-Net output depth = number of **classes** (not "channels" — that's circular).
3. **backbone** = pretrained body, **head** = task-specific final layer.
