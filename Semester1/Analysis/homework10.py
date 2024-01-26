from matplotlib import pyplot as plt
import numpy as np
# Author: Mihai-Dan Crisan 913

def f(x, y, b): 
    return 1/2 * (x**2 + b * y**2)

def grad_f(x, y, b):
    return [x, b * y]

def learning_rate(x, y, b): # sk using the formula from the written homework
    if (b**3 * y**2 - x**2) == 0:
        return (b**2 * y**2 - x**2) / 0.0000000001
    return (b**2 * y**2 - x**2) / (b**3 * y**2 - x**2) 

def grad_descent(x0, y0, b, iterations):
    x = x0
    y = y0
    x_list = [x0]
    y_list = [y0]
    grad = grad_f(x, y, b)

    for i in range(iterations):
        sk = learning_rate(x, y, b)
        x = x * (1 - sk)
        y = y * (1 - b * sk)
        x_list.append(x)
        y_list.append(y)
        grad = grad_f(x, y, b)
    return x_list, y_list

def main():
    b_values = [1, 1/2, 1/5, 1/10, 1/100]
    for b in b_values:
        fig = plt.figure(figsize=(10, 10))
        ax = fig.add_subplot(111, projection='3d')

        x = np.arange(-2, 2, 0.05)
        y = np.arange(-2, 2, 0.05)
        X, Y = np.meshgrid(x, y)
        Z = f(X, Y, b)
        ax.plot_surface(X, Y, Z, cmap='viridis', alpha=0.5)

        x0 = 2
        y0 = 2

        iterations = 10
        x_list, y_list = grad_descent(x0, y0, b, iterations)
        z_list = [f(x, y, b) for x, y in zip(x_list, y_list)]
        ax.plot(x_list, y_list, z_list, 'ro-')

        ax.set_xlabel('x')
        ax.set_ylabel('y')
        ax.set_zlabel('z')
        plt.savefig(f'{b}.png', format='png')
        plt.show()

"""
for b = 1 it gets stuck in the first iteration because the learning rate is 0
As b gets smaller, the learning rate gets bigger and the algorithm converges faster
Graphically it looks like the algorithm is going straight down the slope of the function
and then jumps to the global minimum.
If b were to be 0 the algorithm should jump straight to the global minimum which will be
the entire x axis.
"""

if __name__ == "__main__":
    main()