import numpy as np
from scipy.integrate import solve_ivp

# Gravitational constant
G = 1.0

# Define the equations of motion
def equations_of_motion(t, y):
    # Positions and velocities of the three bodies
    r1 = y[0:2]
    v1 = y[2:4]
    r2 = y[4:6]
    v2 = y[6:8]
    r3 = y[8:10]
    v3 = y[10:12]

    # Masses of the three bodies
    m1 = 1.0
    m2 = 1.0
    m3 = 1.0

    # Calculate distances between bodies
    r12 = np.linalg.norm(r2 - r1)
    r13 = np.linalg.norm(r3 - r1)
    r23 = np.linalg.norm(r3 - r2)

    # Calculate gravitational forces
    f12 = G * m1 * m2 * (r2 - r1) / r12**3
    f13 = G * m1 * m3 * (r3 - r1) / r13**3
    f21 = -f12
    f23 = G * m2 * m3 * (r3 - r2) / r23**3
    f31 = -f13
    f32 = -f23

    # Calculate accelerations
    a1 = (f12 + f13) / m1
    a2 = (f21 + f23) / m2
    a3 = (f31 + f32) / m3

    # Return derivatives
    return np.concatenate((v1, a1, v2, a2, r3, a3))

# Initial conditions
r1_0 = np.array([0.0, 1.0])
v1_0 = np.array([0.5, 0.0])
r2_0 = np.array([1.0, -0.5])
v2_0 = np.array([0.0, 0.5])
r3_0 = np.array([-1.0, -1.0])
v3_0 = np.array([-0.3, -0.2])

# Combine initial conditions into a single array
y0 = np.concatenate((r1_0, v1_0, r2_0, v2_0, r3_0, v3_0))

# Time span
t_span = (0, 10)
t_eval = np.linspace(t_span[0], t_span[1], 1000)

# Solve the equations of motion
sol = solve_ivp(equations_of_motion, t_span, y0, dense_output=True, t_eval=t_eval)

# Extract the positions of the bodies
r1 = sol.y[0:2, :]
r2 = sol.y[4:6, :]
r3 = sol.y[8:10, :]

# Plot the results
import matplotlib.pyplot as plt

plt.plot(r1[0, :], r1[1, :], label='Body 1')
plt.plot(r2[0, :], r2[1, :], label='Body 2')
plt.plot(r3[0, :], r3[1, :], label='Body 3')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Three-Body Problem')
plt.legend()
plt.grid(True)
plt.show()

