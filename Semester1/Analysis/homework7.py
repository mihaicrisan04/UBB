import matplotlib.pyplot as plt
import numpy as np  

EULER = 2.7182818284590452353602874713527

def f(x: float) -> float:
    return EULER ** (-(x ** 2))

def aproximate_area(rect_len: float, y: np.array) -> float:
    return np.sum(y * rect_len)
    
num_points = 50
a = 5

x = np.linspace(-a, a, num_points)
y = f(x)


for i in (1, 2, 5, 10):
    x = np.linspace(-a, a, num_points * i)
    y = f(x)
    rect_len = x[1] - x[0]
    area = aproximate_area(rect_len, y)

    plt.figure(figsize=(16, 10))
    plt.plot(x, y, label=f'Number of points={num_points * i}', color='red', linewidth=3, linestyle='-')
    plt.bar(x - rect_len/2, y, width=rect_len, alpha=0.5, align='edge', label=f'Rectangle width={rect_len}')  
    plt.xlabel('x')
    plt.ylabel('y')
    plt.title('Gaussian function')
    plt.legend()
    plt.show()

    print(f'Number of points: {num_points * i}')
    print(f'area={area}, error={abs(area - np.sqrt(np.pi))}, error%={abs(area - np.sqrt(np.pi)) / np.sqrt(np.pi) * 100}, rect_len={rect_len}, sqrt(pi)={np.sqrt(np.pi)}')
    print()



