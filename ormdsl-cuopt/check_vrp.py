from cuopt import routing
import numpy as np

try:
    import cudf
    USE_CUDF = True
except ImportError:
    USE_CUDF = False

# 简单 TSP
cost_matrix = np.array([
    [0, 10, 15, 20],
    [10, 0, 35, 25],
    [15, 35, 0, 30],
    [20, 25, 30, 0]
], dtype=np.float64)

dm = routing.DataModel(n_locations=4, n_fleet=1)
if USE_CUDF:
    dm.add_cost_matrix(cudf.DataFrame(cost_matrix))
else:
    dm.add_cost_matrix(cost_matrix)

settings = routing.SolverSettings()
settings.max_runtime = 30

solution = routing.Solve(dm, settings)

print('Status:', solution.get_status())

# 获取路线
print('Vehicle count:', solution.get_vehicle_count())
route = solution.get_route()
print('Route:', route)
print('Total cost:', solution.get_total_cost())
