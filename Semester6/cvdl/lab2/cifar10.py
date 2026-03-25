from __future__ import annotations

import pickle
from pathlib import Path

import numpy as np

LABELS = [
    "airplane",
    "automobile",
    "bird",
    "cat",
    "deer",
    "dog",
    "frog",
    "horse",
    "ship",
    "truck",
]

_TRAIN_BATCHES = [f"data_batch_{idx}" for idx in range(1, 6)]
_TEST_BATCH = "test_batch"


def _read_batch(path: str | Path) -> tuple[np.ndarray, np.ndarray]:
    with Path(path).open("rb") as handle:
        batch = pickle.load(handle, encoding="bytes")

    data = batch[b"data"]
    labels = np.asarray(batch[b"labels"], dtype=np.int64)

    images = data.reshape(-1, 3, 32, 32).transpose(0, 2, 3, 1)
    return images, labels


def load_cifar10(root_dir: str | Path) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    root = Path(root_dir)
    if not root.exists():
        raise FileNotFoundError(
            f"CIFAR-10 directory '{root}' was not found. "
            "Download and extract the archive before loading the dataset."
        )

    train_images = []
    train_labels = []
    for batch_name in _TRAIN_BATCHES:
        images, labels = _read_batch(root / batch_name)
        train_images.append(images)
        train_labels.append(labels)

    X_train = np.concatenate(train_images, axis=0)
    y_train = np.concatenate(train_labels, axis=0)
    X_test, y_test = _read_batch(root / _TEST_BATCH)
    return X_train, y_train, X_test, y_test
