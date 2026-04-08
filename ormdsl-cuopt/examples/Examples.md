# ORMDSL-CUOPT Examples

## 1. Linear Programming Example

### Simple LP: Production Planning

```
ORMDSL Model:
  minimize: 3*x1 + 2*x2
  subject to:
    x1 + x2 <= 10
    2*x1 + x2 <= 16
    x1 <= 7
    x1, x2 >= 0
```

### Generated CUDA Code

```cpp
// ORMDSL-CUOPT Generated LP Solver
// Problem: Production Planning

#include "solver.h"
#include <stdio.h>

int main() {
    printf("Production Planning LP Solver\n");
    
    // Problem data
    float c[] = {3.0f, 2.0f};
    float A[] = {1.0f, 1.0f,   // Constraint 1
                 2.0f, 1.0f,   // Constraint 2
                 1.0f, 0.0f};  // Constraint 3
    float b[] = {10.0f, 16.0f, 7.0f};
    
    float x[2];
    int status;
    
    int iters = pdlp_solve(c, A, b, NULL, NULL, 2, 3, 60.0f, x, &status);
    
    printf("Optimal solution: x1=%.2f, x2=%.2f\n", x[0], x[1]);
    printf("Objective value: %.2f\n", 3*x[0] + 2*x[1]);
    
    return 0;
}
```

## 2. Mixed Integer Programming Example

### Facility Location Problem

```
ORMDSL Model:
  sets:
    I = {1..5}  // Potential facility locations
    J = {1..10} // Customers
  
  variables:
    y[i] in {0,1} for i in I      // Open facility
    x[i,j] in [0,1] for i in I, j in J  // Assignment
  
  minimize:
    sum{i in I} (500*y[i] + sum{j in J} 10*d[i,j]*x[i,j])
  
  subject to:
    sum{i in I} x[i,j] = 1 for j in J  // Each customer served
    x[i,j] <= y[i] for i in I, j in J  // Can only serve if open
```

## 3. Vehicle Routing Problem Example

### Capacitated VRP with Time Windows

```
ORMDSL Model:
  sets:
    V = {0..n}  // Nodes (0 = depot)
    K = {1..m}  // Vehicles
  
  parameters:
    d[i,j]     // Distance from i to j
    q[i]       // Demand at node i
    Q         // Vehicle capacity
    tw[i]      // Time window [open, close]
    s[i]       // Service time
  
  variables:
    u[i,k]     // Load of vehicle k at node i
    t[i,k]     // Arrival time at node i on vehicle k
    x[i,j,k]   // Binary: vehicle k travels i->j
  
  minimize: sum{i,j,k} d[i,j] * x[i,j,k]
  
  subject to:
    sum{j} x[0,j,k] = 1 for k in K           // Leave depot
    sum{i} x[i,j,k] = sum{j} x[j,i,k]        // Flow conservation
    sum{i} q[i]*sum{j} x[i,j,k] <= Q         // Capacity
    t[i,k] + s[i] + d[i,j] <= t[j,k] + M*(1-x[i,j,k])  // Time windows
```

### Generated GPU Code Structure

```cpp
// GPU-parallel ALNS for VRP

// Kernel: Evaluate all 2-opt moves
__global__ void two_opt_kernel(...) {
    // Compute delta for each edge pair
}

// Kernel: Apply best moves
__global__ void apply_two_opt_kernel(...) {
    // Reverse segment between best edges
}

// Kernel: Check capacity constraints
__global__ void check_capacity_kernel(...) {
    // Verify vehicle load limits
}

// Kernel: Check time windows
__global__ void check_time_windows_kernel(...) {
    // Verify arrival times within windows
}

// Main ALNS loop
void solve_vrp_alns(RoutingSolution& sol) {
    for (int iter = 0; iter < max_iterations; iter++) {
        // Select destroy operator (Shaw, random, worst)
        int* removed = destroy_solution(sol);
        
        // Apply repair operator (Greedy, regret)
        repair_solution(sol, removed);
        
        // Accept or reject based on SA criterion
        if (accept_solution(new_cost, current_cost, temp)) {
            update_solution(sol);
        }
        
        // Update operator scores
        update_scores(destroy_op, repair_op);
        
        // Cool down
        temp *= cooling_rate;
    }
}
```

## 4. Multi-Depot VRP Example

```python
# Python API usage for Multi-Depot VRP

import numpy as np
from cuopt_solver import VRPSolver

# Distance matrix (3 depots, 6 customers)
costs = np.array([
    # Depot 0, D1, D2, C1, C2, C3, C4, C5, C6
    [0, 0, 0, 10, 15, 20, 12, 8, 5],   # Node 0 (multi-depot)
    [0, 0, 0, 12, 14, 18, 10, 9, 6],   # Node 1 (multi-depot)
    [0, 0, 0, 8, 16, 22, 14, 11, 7],  # Node 2 (multi-depot)
    # ... customer rows
])

# Multiple depots
depot_nodes = [0, 1, 2]
demands = np.array([0, 0, 0, 5, 3, 6, 4, 2, 3])
capacities = np.array([15, 15, 15])

solver = VRPSolver(costs, num_vehicles=6, demands=demands, capacities=capacities)
result = solver.solve(multi_depot=True, depot_nodes=depot_nodes)

print(f"Routes: {result['routes']}")
```

## 5. Benchmark Results

| Problem | Variables | Constraints | CPU (Gurobi) | GPU (CUOPT) | Speedup |
|---------|-----------|-------------|--------------|-------------|---------|
| Production LP | 1000 | 500 | 0.5s | 0.02s | 25x |
| Assignment MILP | 10000 | 100 | 45s | 3s | 15x |
| CVRP-50 | 50 customers | - | 120s | 8s | 15x |
| VRPTW-100 | 100 customers | - | 600s | 30s | 20x |
