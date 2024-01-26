import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def quadratic_function(x, A):
    return 0.5 * x.T @ A @ x  # 0.5 * x^T * A * x where @ is the matrix multiplication operator used in Python 3.5+

def plot_surface_contour_gradient(A, title):
    x = np.linspace(-5, 5, 100)
    y = np.linspace(-5, 5, 100)
    X, Y = np.meshgrid(x, y)
    Z = np.zeros_like(X)

    for i in range(X.shape[0]):
        for j in range(X.shape[1]):
            Z[i, j] = quadratic_function(np.array([X[i, j], Y[i, j]]), A)

    fig = plt.figure(figsize=(8, 8))

    # Plot 3D Surface
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_surface(X, Y, Z, cmap='viridis', alpha=0.5, label='Surface')

    # Plot The level sets
    contour = ax.contour(X, Y, Z, levels=20, cmap='viridis')

    # Plot Gradient at Three Different Points
    for point in [(1, 2), (-2, -3), (3, -2)]:
        gradient = A @ np.array(point)
        ax.quiver(point[0], point[1], quadratic_function(np.array(point), A),
                gradient[0], gradient[1], quadratic_function(gradient, A), 
                color='red', length=50, normalize=True, arrow_length_ratio=0.05,
                label=f'Gradient at ({point[0]}, {point[1]})')

    ax.set_title(f'{title}')
    ax.legend()
    plt.show()

# Matrices for each scenario
A_min = np.array([[2, 0], [0, 3]])
A_max = np.array([[-2, 0], [0, -3]])
A_saddle = np.array([[2, 0], [0, -3]])

# Plot for each scenario
plot_surface_contour_gradient(A_min, "Unique Minimum")
plot_surface_contour_gradient(A_max, "Unique Maximum")
plot_surface_contour_gradient(A_saddle, "Unique Saddle Point")
