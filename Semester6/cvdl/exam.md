A dataset has 950 background patches and 50 foreground patches. You train with standard cross-entropy L = - _ yi log(pi). What is the most likely failure mode?
Select one:
O a. The model overfits to the minority class because its loss terms are individually larger
b. The model learns to always predict "background" and still achieves low average loss
• c. Cross-entropy cannot be minimised with gradient descent on imbalanced data
• d. The model diverges due to vanishing gradients on the majority class

**Answer: b** — with 95% background, "always predict background" gets 95% accuracy and very low average CE. Classic class-imbalance collapse — fix with weighted CE, focal loss, or oversampling.

VGG replaces a single large kernel with a stack of 3x3 convolutions. Two stacked 3x3 convolutions (with stride 1) have the same receptive field as which single convolution?
Select one:
O a. A single 6x6 convolution
O b. A single 5x5 convolution
O c. A single 3x3 convolution
O d. A single 4x4 convolution

**Answer: b** — two stacked 3×3s see a 5×5 region (RF grows by (k−1) per layer: 3 → 3+2 = 5). Same RF as a 5×5 but fewer params (2·9 = 18 vs 25) and an extra non-linearity.
