from __future__ import annotations

from pathlib import Path

import numpy as np
import torch


def softmax(x: torch.Tensor, dim: int = -1) -> torch.Tensor:
    shifted = x - torch.max(x, dim=dim, keepdim=True).values
    exp = torch.exp(shifted)
    return exp / exp.sum(dim=dim, keepdim=True)


def log_softmax(x: torch.Tensor, dim: int = -1) -> torch.Tensor:
    shifted = x - torch.max(x, dim=dim, keepdim=True).values
    return shifted - torch.log(torch.exp(shifted).sum(dim=dim, keepdim=True))


def cross_entropy_loss(log_probs: torch.Tensor, y_true: torch.Tensor) -> torch.Tensor:
    if y_true.ndim == 1:
        return -log_probs[torch.arange(log_probs.shape[0]), y_true.long()].mean()
    return -(y_true.float() * log_probs).sum(dim=1).mean()


class SoftmaxClassifier:
    def __init__(
        self,
        input_shape: int,
        num_classes: int,
        weight_scale: float = 1e-3,
        seed: int | None = None,
    ) -> None:
        self.input_shape = input_shape
        self.num_classes = num_classes
        self.weight_scale = weight_scale
        self.seed = seed
        self.W: torch.Tensor | None = None
        self.initialize()

    def initialize(self) -> None:
        generator = torch.Generator()
        if self.seed is not None:
            generator.manual_seed(self.seed)

        self.W = self.weight_scale * torch.randn(
            self.input_shape + 1,
            self.num_classes,
            dtype=torch.float32,
            generator=generator,
        )
        self.W.requires_grad_(True)

    def _prepare_inputs(self, X: np.ndarray | torch.Tensor) -> torch.Tensor:
        if isinstance(X, torch.Tensor):
            tensor = X.float()
        else:
            tensor = torch.tensor(X, dtype=torch.float32)

        if tensor.ndim == 1:
            tensor = tensor.unsqueeze(0)

        if tensor.shape[1] == self.input_shape:
            bias = torch.ones((tensor.shape[0], 1), dtype=tensor.dtype, device=tensor.device)
            tensor = torch.cat([tensor, bias], dim=1)
        elif tensor.shape[1] != self.input_shape + 1:
            raise ValueError(
                f"Expected inputs with {self.input_shape} features "
                f"(or {self.input_shape + 1} with bias), got {tensor.shape[1]}."
            )

        return tensor

    def _scores(self, X: np.ndarray | torch.Tensor) -> torch.Tensor:
        if self.W is None:
            raise RuntimeError("The classifier weights are not initialized.")
        return self._prepare_inputs(X) @ self.W

    def fit(
        self,
        X_train: np.ndarray | torch.Tensor,
        y_train: np.ndarray | torch.Tensor,
        lr: float = 5e-2,
        epochs: int = 32,
        bs: int = 32,
        reg_strength: float = 0.0,
        shuffle: bool = True,
    ) -> list[dict[str, float]]:
        X_train = self._prepare_inputs(X_train)
        if isinstance(y_train, torch.Tensor):
            y_train = y_train.long()
        else:
            y_train = torch.tensor(y_train, dtype=torch.long)

        if self.W is None:
            self.initialize()

        history: list[dict[str, float]] = []
        num_train = X_train.shape[0]

        for _epoch in range(epochs):
            if shuffle:
                permutation = torch.randperm(num_train)
                X_epoch = X_train[permutation]
                y_epoch = y_train[permutation]
            else:
                X_epoch = X_train
                y_epoch = y_train

            epoch_loss = 0.0
            epoch_correct = 0

            for ii in range((num_train - 1) // bs + 1):
                start_idx = ii * bs
                end_idx = start_idx + bs
                xb = X_epoch[start_idx:end_idx]
                yb = y_epoch[start_idx:end_idx]

                logits = xb @ self.W
                log_probs = log_softmax(logits)
                data_loss = cross_entropy_loss(log_probs, yb)
                reg_loss = reg_strength * torch.sum(self.W[:-1] ** 2)
                loss = data_loss + reg_loss

                loss.backward()

                with torch.no_grad():
                    self.W -= lr * self.W.grad
                    self.W.grad.zero_()

                epoch_loss += loss.item() * xb.shape[0]
                epoch_correct += (logits.argmax(dim=1) == yb).sum().item()

            history.append(
                {
                    "loss": epoch_loss / num_train,
                    "accuracy": epoch_correct / num_train,
                }
            )

        return history

    def predict(self, X: np.ndarray | torch.Tensor) -> np.ndarray:
        with torch.no_grad():
            return self._scores(X).argmax(dim=1).cpu().numpy()

    def predict_proba(self, X: np.ndarray | torch.Tensor) -> np.ndarray:
        with torch.no_grad():
            return softmax(self._scores(X), dim=1).cpu().numpy()

    def save(self, path: str | Path) -> None:
        if self.W is None:
            raise RuntimeError("Cannot save an uninitialized classifier.")

        with Path(path).open("wb") as handle:
            np.save(handle, self.W.detach().cpu().numpy())

    def load(self, path: str | Path) -> None:
        with Path(path).open("rb") as handle:
            weights = np.load(handle)

        self.W = torch.tensor(weights, dtype=torch.float32, requires_grad=True)
