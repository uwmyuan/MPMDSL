# ORMDSL Quick Start Guide

This guide helps you complete your first ORMDSL model in 5 minutes.

---

## 1. Environment Setup

### 1.1 Quick Installation

```bash
# Clone the project
git clone https://github.com/your-repo/ormdsl.git
cd ormdsl

# Install Python dependencies
pip install gurobipy

# Install Scala (optional, for code generation)
conda install -c conda-forge scala=2.13 openjdk
```

### 1.2 Verify Installation

```python
# Save as verify.py
import gurobipy as gp
print(f"Gurobi version: {gp.gurobi.version()}")
print("✓ Installation successful!")
```

```bash
python verify.py
```

---

## 2. First Model: 0-1 Knapsack Problem

### 2.1 Problem Description

```
Given n items, each with value v[i] and weight w[i].
Knapsack capacity is W.
Select items to maximize total value while total weight does not exceed W.

Mathematical Model:
max sum(v[i] * x[i])
s.t. sum(w[i] * x[i]) <= W
     x[i] in {0, 1} for all i
```

### 2.2 Python Implementation

Create file `knapsack.py`:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
0-1 Knapsack Problem - ORMDSL Example
"""

import gurobipy as gp
from gurobipy import GRB

def solve_knapsack():
    """Solve 0-1 knapsack problem"""
    
    # ===== Problem Data =====
    items = ["item1", "item2", "item3", "item4", "item5"]
    profits = {"item1": 10, "item2": 20, "item3": 15, "item4": 25, "item5": 30}
    weights = {"item1": 2, "item2": 4, "item3": 3, "item4": 5, "item5": 6}
    capacity = 10
    
    # ===== Create Model =====
    model = gp.Model("knapsack")
    
    # ===== Decision Variables =====
    # x[i] = 1 if item i is selected, otherwise 0
    x = model.addVars(items, vtype=GRB.BINARY, name="x")
    
    # ===== Objective Function =====
    # Maximize total profit
    model.setObjective(
        gp.quicksum(profits[i] * x[i] for i in items),
        GRB.MAXIMIZE
    )
    
    # ===== Constraints =====
    # Total weight must not exceed knapsack capacity
    model.addConstr(
        gp.quicksum(weights[i] * x[i] for i in items) <= capacity,
        name="weight_capacity"
    )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Output Results =====
    print("=" * 50)
    print("0-1 Knapsack Problem Results")
    print("=" * 50)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nOptimal objective value: {model.objVal}")
        print(f"Solution time: {model.runtime:.4f} seconds")
        print("\nSelected items:")
        for i in items:
            if x[i].x > 0.5:
                print(f"  ✓ {i}: profit={profits[i]}, weight={weights[i]}")
        
        total_weight = sum(weights[i] * x[i].x for i in items)
        print(f"\nTotal weight: {total_weight} / {capacity}")
    else:
        print(f"Solving failed, status: {model.status}")
    
    return model

if __name__ == "__main__":
    solve_knapsack()
```

### 2.3 Run

```bash
python knapsack.py
```

**Expected Output:**

```
==================================================
0-1 Knapsack Problem Results
==================================================

Optimal objective value: 50.0
Solution time: 0.0010 seconds

Selected items:
  ✓ item2: profit=20, weight=4
  ✓ item5: profit=30, weight=6

Total weight: 10 / 10
```

---

## 3. Second Model: Facility Location Problem

### 3.1 Problem Description

```
Select P facilities from m candidate locations to serve n customers,
minimizing total weighted distance.

Mathematical Model:
min sum(d[i][j] * demand[i] * x[i][j])
s.t. sum(y[j] for j in J) = P            # Open exactly P facilities
     sum(x[i][j] for j in J) = 1        # Each customer assigned to exactly one facility
     x[i][j] <= y[j]                     # Can only assign to open facilities
     x[i][j] in {0, 1}, y[j] in {0, 1}
```

### 3.2 Python Implementation

Create file `facility_location.py`:

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
P-Median Facility Location Problem - ORMDSL Example
"""

import gurobipy as gp
from gurobipy import GRB

def solve_facility_location():
    """Solve P-median facility location problem"""
    
    # ===== Problem Data =====
    customers = ["c1", "c2", "c3", "c4", "c5"]
    facilities = ["f1", "f2", "f3", "f4"]
    P = 2  # Number of facilities to open
    
    demand = {"c1": 10, "c2": 15, "c3": 8, "c4": 12, "c5": 5}
    
    distance = {
        ("c1", "f1"): 10, ("c1", "f2"): 15, ("c1", "f3"): 20, ("c1", "f4"): 25,
        ("c2", "f1"): 15, ("c2", "f2"): 10, ("c2", "f3"): 18, ("c2", "f4"): 22,
        ("c3", "f1"): 20, ("c3", "f2"): 18, ("c3", "f3"): 8,  ("c3", "f4"): 16,
        ("c4", "f1"): 25, ("c4", "f2"): 22, ("c4", "f3"): 16, ("c4", "f4"): 10,
        ("c5", "f1"): 18, ("c5", "f2"): 14, ("c5", "f3"): 12, ("c5", "f4"): 15,
    }
    
    # ===== Create Model =====
    model = gp.Model("facility_location")
    
    # ===== Decision Variables =====
    # y[j] = 1 if facility j is opened
    y = model.addVars(facilities, vtype=GRB.BINARY, name="y")
    
    # x[i][j] = 1 if customer i is assigned to facility j
    x = model.addVars(customers, facilities, vtype=GRB.BINARY, name="x")
    
    # ===== Objective Function =====
    # Minimize total weighted distance
    model.setObjective(
        gp.quicksum(distance[i, j] * demand[i] * x[i, j] 
                    for i in customers for j in facilities),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # 1. Open exactly P facilities
    model.addConstr(
        gp.quicksum(y[j] for j in facilities) == P,
        name="num_facilities"
    )
    
    # 2. Each customer assigned to exactly one facility
    for i in customers:
        model.addConstr(
            gp.quicksum(x[i, j] for j in facilities) == 1,
            name=f"assign_{i}"
        )
    
    # 3. Can only assign to open facilities
    for i in customers:
        for j in facilities:
            model.addConstr(x[i, j] <= y[j], name=f"link_{i}_{j}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Output Results =====
    print("=" * 60)
    print("P-Median Facility Location Problem Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nOptimal objective value: {model.objVal}")
        print(f"Solution time: {model.runtime:.4f} seconds")
        
        print("\nOpened facilities:")
        for j in facilities:
            if y[j].x > 0.5:
                print(f"  ✓ {j}")
        
        print("\nCustomer assignments:")
        for i in customers:
            for j in facilities:
                if x[i, j].x > 0.5:
                    print(f"  {i} → {j} (distance: {distance[i, j]})")
    else:
        print(f"Solving failed, status: {model.status}")
    
    return model

if __name__ == "__main__":
    solve_facility_location()
```

### 3.3 Run

```bash
python facility_location.py
```

---

## 4. Third Model: Traveling Salesman Problem (TSP)

### 4.1 Problem Description

```
Given n cities and their distance matrix,
find the shortest path visiting all cities exactly once and returning to start.

Mathematical Model:
min sum(d[i][j] * x[i][j]) 
s.t. sum(x[i][j] for j) = 1           # Leave each city exactly once
     sum(x[i][j] for i) = 1           # Arrive at each city exactly once
     u[i] - u[j] + n * x[i][j] <= n-1  # MTZ subtour elimination constraint
     x[i][j] in {0, 1}
```

### 4.2 Python Implementation

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Traveling Salesman Problem (TSP) - ORMDSL Example
"""

import gurobipy as gp
from gurobipy import GRB

def solve_tsp(n_cities=5):
    """Solve Traveling Salesman Problem"""
    
    import random
    random.seed(42)
    
    cities = list(range(n_cities))
    
    # Generate random distance matrix
    distance = {}
    for i in cities:
        for j in cities:
            if i != j:
                distance[i, j] = random.randint(1, 100)
    
    # ===== Create Model =====
    model = gp.Model("tsp")
    
    # ===== Decision Variables =====
    # x[i][j] = 1 if traveling from city i to city j
    x = model.addVars(cities, cities, vtype=GRB.BINARY, name="x")
    
    # u[i] for subtour elimination (MTZ formulation)
    u = model.addVars(cities, vtype=GRB.CONTINUOUS, lb=0, ub=n_cities, name="u")
    
    # ===== Objective Function =====
    model.setObjective(
        gp.quicksum(distance[i, j] * x[i, j] 
                    for i in cities for j in cities if i != j),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Leave each city exactly once
    for i in cities:
        model.addConstr(
            gp.quicksum(x[i, j] for j in cities if i != j) == 1,
            name=f"leave_{i}"
        )
    
    # Arrive at each city exactly once
    for j in cities:
        model.addConstr(
            gp.quicksum(x[i, j] for i in cities if i != j) == 1,
            name=f"arrive_{j}"
        )
    
    # Subtour elimination constraints (MTZ)
    for i in cities:
        for j in cities:
            if i != j and i != 0 and j != 0:
                model.addConstr(
                    u[i] - u[j] + n_cities * x[i, j] <= n_cities - 1,
                    name=f"subtour_{i}_{j}"
                )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Output Results =====
    print("=" * 50)
    print(f"Traveling Salesman Problem Results (n={n_cities})")
    print("=" * 50)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nShortest path length: {model.objVal}")
        print(f"Solution time: {model.runtime:.4f} seconds")
        
        # Extract path
        path = [0]
        current = 0
        for _ in range(n_cities - 1):
            for j in cities:
                if j not in path and x[current, j].x > 0.5:
                    path.append(j)
                    current = j
                    break
        path.append(0)  # Return to start
        
        print(f"\nOptimal route: {' → '.join(map(str, path))}")
    else:
        print(f"Solving failed, status: {model.status}")
    
    return model

if __name__ == "__main__":
    solve_tsp(5)
```

---

## 5. Using ORMDSL Code Generators

### 5.1 Generate AMPL Code from DSL

```bash
# Compile ORMDSL Scala code
cd ormdsl-intepreter/src
scalac AST2AMPL.scala
scala AST2AMPL
```

### 5.2 Generate Gurobi Code from DSL

```python
# Use ORMDSL Python API
from ormdsl import Model, Variable, Constraint

model = Model("my_problem")
x = model.addVar("x", vtype="binary")
model.setObjective(10 * x, sense="maximize")
model.addConstraint("c1", x <= 1)

# Generate Gurobi code
gurobi_code = model.toGurobi()
print(gurobi_code)
```

---

## 6. Next Steps

Congratulations on completing the quick start! Here's what you can do next:

| Topic | Document |
|-------|----------|
| Learn core concepts in depth | [02-Core-Concepts.md](02-Core-Concepts.md) |
| View complete API | [04-API-Reference.md](04-API-Reference.md) |
| Learn more examples | [07-Examples.md](07-Examples.md) |
| Optimize performance | [08-Best-Practices.md](08-Best-Practices.md) |

---

## 7. FAQ

### Q: The solution shows "optimal" but doesn't match expectations?

A: Check if constraints are correctly defined, ensure constraint logic matches the problem.

### Q: Solving takes too long?

A: Try:
- Set time limit `model.Params.TimeLimit = 3600`
- Set optimality gap `model.Params.MIPGap = 0.001`
- Use heuristic solving `model.Params.Heuristics = 0.5`

### Q: How to handle large-scale problems?

A: Consider:
- Decompose problem into subproblems
- Use column generation or Benders decomposition
- Use specialized decomposition solvers
