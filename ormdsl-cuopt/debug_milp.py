import sys
sys.path.insert(0, '/workspace')
sys.path.insert(0, '/workspace/tests')

from docker_test_gpu import GPUCuOptMILPSolver
import numpy as np

cost = np.array([[9, 2, 7], [6, 4, 3], [5, 8, 1]])
solver = GPUCuOptMILPSolver()
for i in range(3):
    for j in range(3):
        solver.add_variable(f'x{i}{j}', lb=0, ub=1, vtype='B')
solver.set_objective(cost.flatten(), sense='minimize')

# 行约束 (等式)
for i in range(3):
    row = np.zeros(9)
    row[i*3:(i+1)*3] = 1
    print(f'Row {i}:', row)
    solver.add_constraint(row.reshape(1, -1), np.array([1.0]), sense="==")

# 列约束 (等式)
for j in range(3):
    col = np.zeros(9)
    col[j::3] = 1
    print(f'Col {j}:', col)
    solver.add_constraint(col.reshape(1, -1), np.array([1.0]), sense="==")

print('\nA_values:', solver._A_values)
print('A_indices:', solver._A_indices)
print('A_offsets:', solver._A_offsets)
print('b_upper:', solver._b_upper)
print('b_lower:', solver._b_lower)

result = solver.solve(time_limit=30.0)
print('\nStatus:', result["status"])
print('Objective:', result["objective"])
