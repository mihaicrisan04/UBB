import numpy as np
import matplotlib.pyplot as plt

# the formula for the p-norm is:
# ||x||_p = (|x_1|^p + |x_2|^p + ... + |x_n|^p)^(1/p)
# where x is a vector in R^n
# here we have x = (x, y) in R^2
# so the formula becomes:
# ||x||_p = (|x|^p + |y|^p)^(1/p)
def is_inside_unit_ball(x, y, p):
    return (np.abs(x)**p + np.abs(y)**p)**(1/p) <= 1

def plot_random_points_in_unit_ball(p, num_points=1000):
    inside_points_x = []
    inside_points_y = []
    outside_points_x = []
    outside_points_y = []

    for _ in range(num_points):
        x = np.random.uniform(-1, 1)
        y = np.random.uniform(-1, 1)

        if is_inside_unit_ball(x, y, p):
            inside_points_x.append(x)
            inside_points_y.append(y)
        else:
            outside_points_x.append(x)
            outside_points_y.append(y)

    plt.scatter(inside_points_x, inside_points_y, label=f'p = {p}', color='red')
    plt.scatter(outside_points_x, outside_points_y, label=f'p = {p}', color='grey')

p_values = [1.25, 1.5, 3, 8]

# Plot random points inside the unit ball for each value of p
for p in p_values:
    plt.figure(figsize=(8, 8))  
    plot_random_points_in_unit_ball(p, 2500)
    plt.title('Random Points in Unit Ball in R^2 for p-norm = ' + str(p))
    plt.xlabel('x')
    plt.ylabel('y')
    plt.legend()
    plt.grid(True)
    plt.axis('equal')
    plt.show()
