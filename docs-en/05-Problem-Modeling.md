# ORMDSL Problem Modeling Guide

This document introduces how to model various classic optimization problems using ORMDSL.

---

## 1. Linear Programming (LP)

### 1.1 Problem Description

The simplest optimization problem, with both objective function and constraints being linear.

```
max cᵀx
s.t. Ax ≤ b
     x ≥ 0
```

### 1.2 Code Implementation

```python
"""
Linear Programming Example: Profit Maximization
A factory produces two products, each requiring labor and raw materials.
Given resource constraints, find the production plan with maximum profit.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_linear_programming():
    # ===== Problem Data =====
    products = ["P1", "P2"]
    profit = {"P1": 30, "P2": 40}  # Unit profit
    resources = ["labor", "material"]  # Resource types
    
    # Resource consumption matrix (products x resources)
    consumption = {
        ("P1", "labor"): 1,     ("P1", "material"): 2,
        ("P2", "labor"): 2,    ("P2", "material"): 1,
    }
    
    # Resource availability
    capacity = {"labor": 100, "material": 80}
    
    # ===== Create Model =====
    model = gp.Model("linear_programming")
    
    # ===== Decision Variables =====
    # x[i] = production quantity of product i
    x = model.addVars(products, vtype=GRB.CONTINUOUS, lb=0, name="x")
    
    # ===== Objective Function =====
    # Maximize total profit
    model.setObjective(
        gp.quicksum(profit[i] * x[i] for i in products),
        GRB.MAXIMIZE
    )
    
    # ===== Constraints =====
    # Each resource consumption must not exceed availability
    for r in resources:
        model.addConstr(
            gp.quicksum(consumption[i, r] * x[i] for i in products) <= capacity[r],
            name=f"capacity_{r}"
        )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Optimal profit: {model.objVal}")
        print("Production plan:")
        for i in products:
            print(f"  {i}: {x[i].x}")
    
    return model

if __name__ == "__main__":
    solve_linear_programming()
```

---

## 2. Integer Programming (IP / MIP)

### 2.1 Mixed Integer Programming (MIP)

```python
"""
Mixed Integer Programming Example: Facility Location
Decide which facilities to open to minimize total cost.
Opening cost is a fixed cost plus operating cost.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_facility_location():
    # ===== Problem Data =====
    potential_facilities = ["F1", "F2", "F3"]
    customers = ["C1", "C2", "C3", "C4"]
    
    # Opening cost (fixed cost)
    fixed_cost = {"F1": 1000, "F2": 800, "F3": 1200}
    
    # Operating cost (unit cost)
    unit_cost = {"F1": 5, "F2": 6, "F3": 4}
    
    # Customer demand
    demand = {"C1": 50, "C2": 80, "C3": 60, "C4": 70}
    
    # ===== Create Model =====
    model = gp.Model("facility_location_mip")
    
    # ===== Decision Variables =====
    # y[j] = 1 if facility j is opened
    y = model.addVars(potential_facilities, vtype=GRB.BINARY, name="y")
    
    # x[j] = quantity served by facility j to customers
    x = model.addVars(customers, potential_facilities, 
                      vtype=GRB.CONTINUOUS, lb=0, name="x")
    
    # ===== Objective Function =====
    # Minimize total cost = opening cost + operating cost
    model.setObjective(
        gp.quicksum(fixed_cost[j] * y[j] for j in potential_facilities) +
        gp.quicksum(unit_cost[j] * x[i, j] for i in customers for j in potential_facilities),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Meet all customer demands
    for i in customers:
        model.addConstr(
            gp.quicksum(x[i, j] for j in potential_facilities) >= demand[i],
            name=f"demand_{i}"
        )
    
    # Can only supply from opened facilities
    for i in customers:
        for j in potential_facilities:
            model.addConstr(x[i, j] <= demand[i] * y[j], name=f"link_{i}_{j}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Optimal total cost: {model.objVal}")
        print("Opened facilities:")
        for j in potential_facilities:
            if y[j].x > 0.5:
                print(f"  {j}: cost {fixed_cost[j]}")
        print("\nSupply plan:")
        for i in customers:
            for j in potential_facilities:
                if x[i, j].x > 0.1:
                    print(f"  {i} ← {j}: {x[i, j].x}")
    
    return model
```

---

## 3. Network Flow Problems

### 3.1 Minimum Cost Flow

```python
"""
Minimum Cost Flow Problem: Material Distribution
Transport materials from multiple warehouses to multiple customers,
finding the minimum transportation cost plan.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_min_cost_flow():
    # ===== Problem Data =====
    warehouses = ["W1", "W2"]
    customers = ["C1", "C2", "C3"]
    
    # Warehouse supply
    supply = {"W1": 100, "W2": 150}
    
    # Customer demand
    demand = {"C1": 60, "C2": 80, "C3": 90}
    
    # Unit transportation cost
    cost = {
        ("W1", "C1"): 4, ("W1", "C2"): 6, ("W1", "C3"): 9,
        ("W2", "C1"): 5, ("W2", "C2"): 3, ("W2", "C3"): 7,
    }
    
    # ===== Create Model =====
    model = gp.Model("min_cost_flow")
    
    # ===== Decision Variables =====
    # flow[i,j] = quantity transported from warehouse i to customer j
    flow = model.addVars(warehouses, customers, 
                         vtype=GRB.CONTINUOUS, lb=0, name="flow")
    
    # ===== Objective Function =====
    # Minimize total transportation cost
    model.setObjective(
        gp.quicksum(cost[i, j] * flow[i, j] 
                    for i in warehouses for j in customers),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Warehouse shipment does not exceed supply
    for i in warehouses:
        model.addConstr(
            gp.quicksum(flow[i, j] for j in customers) <= supply[i],
            name=f"supply_{i}"
        )
    
    # Customer receives demanded quantity
    for j in customers:
        model.addConstr(
            gp.quicksum(flow[i, j] for i in warehouses) >= demand[j],
            name=f"demand_{j}"
        )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Minimum transportation cost: {model.objVal}")
        print("Distribution plan:")
        for i in warehouses:
            for j in customers:
                if flow[i, j].x > 0:
                    print(f"  {i} → {j}: {flow[i, j].x} (cost: {cost[i,j]*flow[i,j].x})")
    
    return model
```

---

## 4. Combinatorial Optimization

### 4.1 Set Covering Problem

```python
"""
Set Covering Problem: Emergency Facility Location
Select the minimum number of facility locations to cover all demand points.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_set_covering():
    # ===== Problem Data =====
    facilities = ["F1", "F2", "F3", "F4", "F5"]
    demand_points = ["D1", "D2", "D3", "D4", "D5", "D6"]
    
    # Facility construction cost
    cost = {"F1": 10, "F2": 15, "F3": 8, "F4": 12, "F5": 9}
    
    # Coverage relationship (which demand points each facility can cover)
    covers = {
        "F1": ["D1", "D2", "D3"],
        "F2": ["D2", "D3", "D4"],
        "F3": ["D3", "D4", "D5"],
        "F4": ["D4", "D5", "D6"],
        "F5": ["D1", "D5", "D6"],
    }
    
    # ===== Create Model =====
    model = gp.Model("set_covering")
    
    # ===== Decision Variables =====
    # y[j] = 1 if facility j is selected
    y = model.addVars(facilities, vtype=GRB.BINARY, name="y")
    
    # ===== Objective Function =====
    # Minimize total construction cost
    model.setObjective(
        gp.quicksum(cost[j] * y[j] for j in facilities),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Each demand point covered by at least one facility
    for d in demand_points:
        model.addConstr(
            gp.quicksum(y[j] for j in facilities if d in covers[j]) >= 1,
            name=f"cover_{d}"
        )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Minimum total cost: {model.objVal}")
        print("Selected facilities:")
        for j in facilities:
            if y[j].x > 0.5:
                print(f"  {j}: covers {covers[j]}")
    
    return model
```

### 4.2 Set Partitioning Problem

```python
"""
Set Partitioning Problem: Simplified Vehicle Routing
Partition demand points into non-overlapping groups,
each served by one vehicle.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_set_partitioning():
    # ===== Problem Data =====
    clients = ["A", "B", "C", "D"]
    vehicles = 2  # Number of available vehicles
    
    # Vehicle capacity
    capacity = 3
    
    # Client demand
    demand = {"A": 1, "B": 2, "C": 1, "D": 2}
    
    # Predefined routes and their costs
    routes = [
        {"clients": ["A"], "cost": 10},
        {"clients": ["B"], "cost": 12},
        {"clients": ["C"], "cost": 11},
        {"clients": ["D"], "cost": 13},
        {"clients": ["A", "B"], "cost": 18},
        {"clients": ["A", "C"], "cost": 17},
        {"clients": ["B", "D"], "cost": 20},
        {"clients": ["C", "D"], "cost": 19},
        {"clients": ["A", "B", "C"], "cost": 25},
        {"clients": ["B", "C", "D"], "cost": 27},
    ]
    
    # ===== Create Model =====
    model = gp.Model("set_partitioning")
    
    # ===== Decision Variables =====
    # x[k] = 1 if route k is selected
    x = model.addVars(range(len(routes)), vtype=GRB.BINARY, name="x")
    
    # ===== Objective Function =====
    # Minimize total cost
    model.setObjective(
        gp.quicksum(routes[k]["cost"] * x[k] for k in range(len(routes))),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Each client served exactly once
    for c in clients:
        model.addConstr(
            gp.quicksum(x[k] for k in range(len(routes)) if c in routes[k]["clients"]) == 1,
            name=f"serve_{c}"
        )
    
    # Use no more than specified number of vehicles
    model.addConstr(
        gp.quicksum(x[k] for k in range(len(routes))) <= vehicles,
        name="vehicle_limit"
    )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Minimum total cost: {model.objVal}")
        print("Selected routes:")
        for k in range(len(routes)):
            if x[k].x > 0.5:
                print(f"  Route {k+1}: {routes[k]['clients']} (cost: {routes[k]['cost']})")
    
    return model
```

---

## 5. Scheduling Problems

### 5.1 Staff Scheduling

```python
"""
Staff Scheduling Problem
Determine the number of employees needed each day,
minimizing cost while meeting demand.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_staff_scheduling():
    # ===== Problem Data =====
    days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    
    # Minimum employee demand per day
    demand = {"Mon": 10, "Tue": 15, "Wed": 12, "Thu": 14, 
              "Fri": 18, "Sat": 8, "Sun": 6}
    
    # Employees work 5 days, rest 2 days
    WORK_DAYS = 5
    REST_DAYS = 2
    
    # Employee cost
    daily_cost = 100
    overtime_cost = 150
    
    # ===== Create Model =====
    model = gp.Model("staff_scheduling")
    
    # ===== Decision Variables =====
    # x[d] = number of employees working on day d
    x = model.addVars(days, vtype=GRB.INTEGER, lb=0, name="x")
    
    # y = number of overtime employees
    y = model.addVar(vtype=GRB.INTEGER, lb=0, name="y")
    
    # ===== Objective Function =====
    # Minimize total cost
    model.setObjective(
        daily_cost * gp.quicksum(x[d] for d in days) + 
        overtime_cost * y,
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Meet daily demand (base employees + overtime)
    for d in days:
        model.addConstr(x[d] + y >= demand[d], name=f"demand_{d}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Minimum total cost: {model.objVal}")
        print("Daily staffing:")
        for d in days:
            base = int(x[d].x)
            over = int(y.x) if d in ["Fri", "Sat"] else 0
            print(f"  {d}: base {base} + overtime {over} = {base + over} (demand: {demand[d]})")
    
    return model
```

---

## 6. Bin Packing Problems

### 6.1 One-Dimensional Bin Packing

```python
"""
One-Dimensional Bin Packing Problem
Pack items into the minimum number of bins.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_bin_packing():
    # ===== Problem Data =====
    items = [f"item_{i}" for i in range(10)]
    item_weights = [3, 5, 2, 8, 4, 6, 1, 7, 3, 5]
    bin_capacity = 10
    
    # ===== Create Model =====
    model = gp.Model("bin_packing")
    
    # Big-M value (upper bound on number of bins)
    n_bins = len(items)
    
    # ===== Decision Variables =====
    # x[i,k] = 1 if item i is packed in bin k
    x = model.addVars(items, range(n_bins), vtype=GRB.BINARY, name="x")
    
    # y[k] = 1 if bin k is used
    y = model.addVars(range(n_bins), vtype=GRB.BINARY, name="y")
    
    # ===== Objective Function =====
    # Minimize number of bins used
    model.setObjective(
        gp.quicksum(y[k] for k in range(n_bins)),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Each item must be packed in one bin
    for i in items:
        idx = items.index(i)
        model.addConstr(
            gp.quicksum(x[i, k] for k in range(n_bins)) == 1,
            name=f"assign_{i}"
        )
    
    # Bin capacity constraint
    for k in range(n_bins):
        model.addConstr(
            gp.quicksum(items.index(i) * item_weights[items.index(i)] * x[i, k] 
                        for i in items) <= bin_capacity * y[k],
            name=f"capacity_{k}"
        )
    
    # Bins used in order (symmetry breaking)
    for k in range(n_bins - 1):
        model.addConstr(y[k] >= y[k + 1], name=f"ordering_{k}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    if model.status == GRB.OPTIMAL:
        print(f"Minimum number of bins: {model.objVal}")
        print("Packing plan:")
        for k in range(n_bins):
            bin_items = [i for i in items if x[i, k].x > 0.5]
            if bin_items:
                weight = sum(item_weights[items.index(i)] for i in bin_items)
                print(f"  Bin {k+1}: {bin_items} (total weight: {weight}/{bin_capacity})")
    
    return model
```

---

## 7. Modeling Techniques

### 7.1 Linearization Techniques

#### Product Linearization

```python
# If z = x * y and x, y are binary variables
# Linearize as:
# z <= x
# z <= y
# z >= x + y - 1

model.addConstr(z <= x)
model.addConstr(z <= y)
model.addConstr(z >= x + y - 1)
```

#### Implication Constraints

```python
# if x == 1 then y == 0
# Linearize as:
# y <= 1 - x

model.addConstr(y <= 1 - x)
```

#### Absolute Value Constraints

```python
# |expr| <= M
# Equivalent to:
# expr <= M
# -expr <= M

model.addConstr(expr <= M)
model.addConstr(-expr <= M)
```

### 7.2 Symmetry Breaking

```python
# Bin ordering constraint
for k in range(n_bins - 1):
    model.addConstr(y[k] >= y[k + 1])

# Index ordering constraint
for i in range(n):
    for j in range(i + 1, n):
        model.addConstr(x[i] <= x[j] + M * (1 - y[i]))
```

### 7.3 Pre-solve

```python
# Filter impossible combinations before modeling
feasible_pairs = [(i, j) for i in items for j in bins 
                  if weight[i] <= capacity[j]]

x = model.addVars(feasible_pairs, vtype=GRB.BINARY)
```
