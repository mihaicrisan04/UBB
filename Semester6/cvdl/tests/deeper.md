# Test — Slightly Deeper

Harder than the basics set: pushes on formulas, exact PyTorch names, edge cases.
Take this **after** you've consistently scored well on the basics tests.

---

## Part A — CNNs (Lab 3)

1. Pooling layers have ______ learnable parameters and operate independently across each input ______.

   Answer: ______

2. The output width of a convolution is given by `W_out = (W_in - F + 2P) / ______ + 1`.

   Answer: ______

3. Two popular low-pass filters used in classical image processing are the ______ filter and the ______ filter.

   Answer: ______

4. To freeze a parameter so it is not updated during backprop, you set `param.______ = False`.

   Answer: ______

5. The occlusion sensitivity technique covers small regions with a ______ patch and measures how much the predicted score ______.

   Answer: ______

6. A ______ sweep tries every combination from a small set of values, while a ______ sweep samples combinations from ranges and runs a fixed budget.

   Answer: ______

7. During evaluation we usually wrap inference in `torch.______()` to skip gradient tracking, and we switch the model to `______` mode so layers like dropout and batch norm behave correctly.

   Answer: ______

---

## Part B — Semantic Segmentation (Lab 4)

8. When applying geometric augmentations to a segmentation sample, the same transformation must be applied to both the image and the ______, otherwise supervision breaks.

   Answer: ______

9. The output of a U-Net segmentation model is a volume with depth ______, where each channel stores the probability map for one ______.

   Answer: ______

10. At the very end of a U-Net, a ______×______ convolution is applied to map decoder features into per-class scores.

    Answer: ______

11. Two examples of unlearned (parameter-free) upsampling are ______-neighbour and ______ interpolation.

    Answer: ______

12. The Dice loss is defined as L_Dice = 1 − ______ .

    Answer: ______

13. ______ accuracy is a misleading metric on segmentation datasets that suffer from class ______.

    Answer: ______

14. To remove small isolated noisy regions from a predicted mask, we can apply ______-______ analysis (available in OpenCV).

    Answer: ______

---

## Part C — Transformers (Lab 5)

15. Self-attention behaves like message passing on a fully ______ graph, where every token is a ______ that exchanges info with every other one.

    Answer: ______

16. In multi-head attention, instead of one set of Q/K/V projections, the model runs ______ of them in ______, then concatenates their outputs.

    Answer: ______

17. In sinusoidal positional encodings, even dimensions use the ______ function and odd dimensions use the ______ function.

    Answer: ______

18. Single-layer attention maps don't show the full information flow across the network; attention ______ composes attention matrices across all layers to fix this.

    Answer: ______

19. The three common strategies for adapting a pretrained ViT to a new task are: linear ______ (frozen backbone), full ______ (everything trainable), and ______ (training only small low-rank updates).

    Answer: ______

20. In LoRA, instead of updating a full D×D matrix, the update is decomposed as ΔW = B · A, where the inner dimension r (the ______) is much ______ than D.

    Answer: ______

---

When done, tell me and I'll grade.
