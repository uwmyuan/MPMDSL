# ORMDSL Examples Manual

This document provides complete example code for learning and reference.

---

## Table of Contents

1. [Basic Problems](#1-basic-problems)
2. [Network Optimization](#2-network-optimization)
3. [Scheduling Problems](#3-scheduling-problems)
4. [Advanced Applications](#4-advanced-applications)

---

## 1. Basic Problems

### 1.1 Production Planning Problem

```python
#!/usr/bin/env python3
"""
Production Planning Problem
A factory produces multiple products, each with profit and resource consumption.
Determine optimal production quantities.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_production_planning():
    # ===== Problem Data =====
    products = ["P1", "P2", "P3", "P4"]
    
    # Product profit ($/unit)
    profit = {"P1": 30, "P2": 40, "P3": 35, "P4": 25}
    
    # Resource consumption (unit/unit of product)
    resources = ["labor", "materials", "machine_time"]
    consumption = {
        ("P1", "labor"): 1,        ("P1", "materials"): 2,    ("P1", "machine_time"): 2,
        ("P2", "labor"): 2,        ("P2", "materials"): 1,    ("P2", "machine_time"): 3,
        ("P3", "labor"): 1.5,      ("P3", "materials"): 2,    ("P3", "machine_time"): 2,
        ("P4", "labor"): 1,        ("P4", "materials"): 3,    ("P4", "machine_time"): 1,
    }
    
    # Resource availability
    capacity = {"labor": 100, "materials": 150, "machine_time": 120}
    
    # Maximum production per product
    max_production = {"P1": 50, "P2": 40, "P3": 60, "P4": 30}
    
    # ===== Create Model =====
    model = gp.Model("production_planning")
    
    # ===== Decision Variables =====
    x = model.addVars(products, vtype=GRB.CONTINUOUS, lb=0, name="x")
    
    # ===== Objective Function =====
    model.setObjective(
        gp.quicksum(profit[p] * x[p] for p in products),
        GRB.MAXIMIZE
    )
    
    # ===== Constraints =====
    # Resource constraints
    for r in resources:
        model.addConstr(
            gp.quicksum(consumption[p, r] * x[p] for p in products) <= capacity[r],
            name=f"resource_{r}"
        )
    
    # Maximum production constraints
    for p in products:
        model.addConstr(x[p] <= max_production[p], name=f"max_{p}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Production Planning Problem Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nOptimal profit: ${model.objVal}")
        print(f"Solution time: {model.runtime:.4f}s")
        print("\nOptimal production plan:")
        for p in products:
            print(f"  {p}: {x[p].x:.1f} units")
        
        print("\nResource utilization:")
        for r in resources:
            used = sum(consumption[p, r] * x[p].x for p in products)
            print(f"  {r}: {used:.1f} / {capacity[r]} ({100*used/capacity[r]:.1f}%)")
    
    return model

if __name__ == "__main__":
    solve_production_planning()
```

### 1.2 Portfolio Optimization

```python
#!/usr/bin/env python3
"""
Portfolio Optimization Problem
Select investment portfolio to maximize returns under risk constraints.
"""

import gurobipy as gp
from gurobipy import GRB
import random

def solve_portfolio_optimization():
    random.seed(42)
    
    # ===== Problem Data =====
    investments = [f"I{i}" for i in range(10)]
    n_assets = len(investments)
    
    # Expected returns (%)
    returns = {inv: random.uniform(5, 15) for inv in investments}
    
    # Covariance matrix (simplified random)
    cov = {}
    for i in investments:
        for j in investments:
            if i == j:
                cov[i, j] = random.uniform(1, 5)
            else:
                cov[i, j] = random.uniform(-1, 1)
    
    # Maximum investment risk (variance upper bound)
    max_risk = 10
    
    # Maximum/minimum investment amount
    max_allocation = 0.3  # Single item not exceeding 30%
    min_allocation = 0.05  # Minimum 5%
    total_budget = 1000000  # Total investment amount
    
    # ===== Create Model =====
    model = gp.Model("portfolio_optimization")
    
    # ===== Decision Variables =====
    # x[i] = proportion invested in project i
    x = model.addVars(investments, vtype=GRB.CONTINUOUS, lb=0, ub=max_allocation, name="x")
    
    # ===== Objective Function =====
    # Maximize expected return
    model.setObjective(
        gp.quicksum(returns[i] * x[i] for i in investments),
        GRB.MAXIMIZE
    )
    
    # ===== Constraints =====
    # Total investment proportion = 1
    model.addConstr(gp.quicksum(x[i] for i in investments) == 1, name="budget")
    
    # Risk constraint (portfolio variance)
    model.addConstr(
        gp.quicksum(cov[i, j] * x[i] * x[j] for i in investments for j in investments) 
        <= max_risk,
        name="risk_constraint"
    )
    
    # Invest in at least 5 projects
    model.addConstr(
        gp.quicksum(x[i] >= min_allocation for i in investments) >= 5,
        name="diversification"
    )
    
    # ===== Solve =====
    model.Params.MIPGap = 0.001
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Portfolio Optimization Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nOptimal expected return: {model.objVal:.2f}%")
        print(f"Portfolio variance: {sum(cov[i,j]*x[i].x*x[j].x for i in investments for j in investments):.2f}")
        print("\nRecommended portfolio:")
        selected = [(i, x[i].x) for i in investments if x[i].x > 0.01]
        selected.sort(key=lambda t: t[1], reverse=True)
        for inv, allocation in selected:
            print(f"  {inv}: {allocation*100:.1f}% (expected return: {returns[inv]:.2f}%)")
        print(f"\nTotal investment: ${total_budget * sum(x[i].x for i in investments):,.0f}")
    
    return model

if __name__ == "__main__":
    solve_portfolio_optimization()
```

---

## 2. Network Optimization

### 2.1 Maximum Flow Problem

```python
#!/usr/bin/env python3
"""
Maximum Flow Problem
Maximum possible flow from source to sink.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_max_flow():
    # ===== Problem Data =====
    nodes = ["s", "v1", "v2", "v3", "v4", "t"]
    source = "s"
    sink = "t"
    
    # Edge capacity
    capacity = {
        ("s", "v1"): 10, ("s", "v2"): 8,
        ("v1", "v2"): 5,  ("v1", "v3"): 8,
        ("v2", "v1"): 3,  ("v2", "v4"): 6,
        ("v3", "v2"): 4,  ("v3", "t"): 9,
        ("v4", "v3"): 5,  ("v4", "t"): 7,
    }
    
    # ===== Create Model =====
    model = gp.Model("max_flow")
    
    # ===== Decision Variables =====
    # flow[i,j] = flow from i to j
    flow = model.addVars(capacity.keys(), vtype=GRB.CONTINUOUS, lb=0, name="flow")
    
    # ===== Objective Function =====
    # Maximize flow into sink
    model.setObjective(
        gp.quicksum(flow[source, j] for i, j in capacity.keys() if i == source),
        GRB.MAXIMIZE
    )
    
    # ===== Constraints =====
    # Capacity constraints
    for (i, j), cap in capacity.items():
        model.addConstr(flow[i, j] <= cap, name=f"cap_{i}_{j}")
    
    # Flow conservation constraints (except source and sink)
    for k in nodes:
        if k != source and k != sink:
            inflow = gp.quicksum(flow[i, k] for i, j in capacity.keys() if j == k)
            outflow = gp.quicksum(flow[k, j] for i, j in capacity.keys() if i == k)
            model.addConstr(inflow == outflow, name=f"flow_cons_{k}")
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Maximum Flow Problem Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nMaximum flow: {model.objVal}")
        print("\nFlow distribution:")
        for (i, j), cap in capacity.items():
            f = flow[i, j].x
            if f > 0:
                print(f"  {i} → {j}: {f} / {cap}")
    
    return model

if __name__ == "__main__":
    solve_max_flow()
```

### 2.2 Shortest Path Problem

```python
#!/usr/bin/env python3
"""
Shortest Path Problem
Find the shortest path from source to sink.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_shortest_path():
    # ===== Problem Data =====
    nodes = ["A", "B", "C", "D", "E", "F", "G"]
    
    # Edges and distances
    edges = [
        ("A", "B", 4), ("A", "C", 2), ("A", "D", 5),
        ("B", "C", 1), ("B", "E", 4),
        ("C", "D", 2), ("C", "E", 3), ("C", "F", 6),
        ("D", "F", 3),
        ("E", "G", 2), ("F", "G", 2),
    ]
    
    source = "A"
    sink = "G"
    
    # ===== Create Model =====
    model = gp.Model("shortest_path")
    
    # ===== Decision Variables =====
    # x[i,j] = 1 if edge (i,j) is selected
    x = model.addVars(
        [(e[0], e[1]) for e in edges],
        vtype=GRB.BINARY,
        name="x"
    )
    
    # ===== Objective Function =====
    # Minimize total distance
    model.setObjective(
        gp.quicksum(dist * x[i, j] for i, j, dist in edges),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Flow conservation
    for k in nodes:
        if k == source:
            # Source: out-degree - in-degree = 1
            model.addConstr(
                gp.quicksum(x[k, j] for i, j, _ in edges if i == k) -
                gp.quicksum(x[i, k] for i, j, _ in edges if j == k) == 1,
                name=f"source_{k}"
            )
        elif k == sink:
            # Sink: out-degree - in-degree = -1
            model.addConstr(
                gp.quicksum(x[k, j] for i, j, _ in edges if i == k) -
                gp.quicksum(x[i, k] for i, j, _ in edges if j == k) == -1,
                name=f"sink_{k}"
            )
        else:
            # Intermediate nodes: flow conservation
            model.addConstr(
                gp.quicksum(x[k, j] for i, j, _ in edges if i == k) ==
                gp.quicksum(x[i, k] for i, j, _ in edges if j == k),
                name=f"flow_{k}"
            )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Shortest Path Problem Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nShortest distance: {model.objVal}")
        
        # Reconstruct path
        path = [source]
        current = source
        edge_dict = {(e[0], e[1]): dist for e in edges}
        
        while current != sink:
            for i, j, d in edges:
                if i == current and x[i, j].x > 0.5:
                    path.append(j)
                    current = j
                    break
        
        print(f"\nShortest path: {' → '.join(path)}")
    
    return model

if __name__ == "__main__":
    solve_shortest_path()
```

---

## 3. Scheduling Problems

### 3.1 Job Shop Scheduling

```python
#!/usr/bin/env python3
"""
Job Shop Scheduling Problem
Multiple jobs, each with multiple operations, each operation requires specific machine.
Determine operation processing order to minimize total completion time.
"""

import gurobipy as gp
from gurobipy import GRB

def solve_job_shop():
    # ===== Problem Data =====
    jobs = ["J1", "J2", "J3"]
    machines = ["M1", "M2", "M3"]
    
    # Operation data: (job, operation, machine, processing_time)
    operations = [
        # Job1: M1(3) → M2(2) → M3(4)
        ("J1", 1, "M1", 3), ("J1", 2, "M2", 2), ("J1", 3, "M3", 4),
        # Job2: M2(4) → M1(3) → M3(2)
        ("J2", 1, "M2", 4), ("J2", 2, "M1", 3), ("J2", 3, "M3", 2),
        # Job3: M1(2) → M3(3) → M2(5)
        ("J3", 1, "M1", 2), ("J3", 2, "M3", 3), ("J3", 3, "M2", 5),
    ]
    
    # ===== Create Model =====
    model = gp.Model("job_shop")
    
    # Parameter
    H = 1000  # Time upper bound
    
    # ===== Decision Variables =====
    # start[j,op] = start time of operation (j,op)
    start = {}
    for j, op, m, p in operations:
        start[j, op] = model.addVar(lb=0, vtype=GRB.CONTINUOUS, name=f"start_{j}_{op}")
    
    # Cmax = total completion time
    Cmax = model.addVar(lb=0, vtype=GRB.CONTINUOUS, name="Cmax")
    
    # ===== Objective Function =====
    model.setObjective(Cmax, GRB.MINIMIZE)
    
    # ===== Constraints =====
    # Operation sequence constraints (same job)
    for j in jobs:
        ops = [(op, m, p) for jj, op, m, p in operations if jj == j]
        ops.sort(key=lambda x: x[0])
        for i in range(len(ops) - 1):
            op1, m1, p1 = ops[i]
            op2, m2, p2 = ops[i + 1]
            model.addConstr(
                start[j, op1] + p1 <= start[j, op2],
                name=f"seq_{j}_{op1}_{op2}"
            )
    
    # Machine conflict constraints (same machine cannot process simultaneously)
    for m in machines:
        ops_on_m = [(j, op, p) for j, op, mm, p in operations if mm == m]
        for i in range(len(ops_on_m)):
            for j in range(i + 1, len(ops_on_m)):
                j1, op1, p1 = ops_on_m[i]
                j2, op2, p2 = ops_on_m[j]
                
                # Big-M constraint to avoid simultaneous processing
                model.addConstr(
                    start[j1, op1] + p1 <= start[j2, op2] + H * (1 - 1),
                    name=f"disj1_{j1}_{op1}_{j2}_{op2}"
                )
                model.addConstr(
                    start[j2, op2] + p2 <= start[j1, op1],
                    name=f"disj2_{j1}_{op1}_{j2}_{op2}"
                )
    
    # Total completion time constraint
    for j, op, m, p in operations:
        if op == max(o for jj, o, mm, pp in operations if jj == j):
            model.addConstr(
                start[j, op] + p <= Cmax,
                name=f"makespan_{j}"
            )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Job Shop Scheduling Problem Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nOptimal makespan: {model.objVal}")
        print("\nOperation start times:")
        for j in jobs:
            ops = [(op, m, p) for jj, op, m, p in operations if jj == j]
            ops.sort(key=lambda x: x[0])
            for op, m, p in ops:
                s = start[j, op].x
                print(f"  {j}, op{op} (machine {m}, time {p}): start={s:.1f}, finish={s+p:.1f}")
    
    return model

if __name__ == "__main__":
    solve_job_shop()
```

---

## 4. Advanced Applications

### 4.1 Two-Stage Stochastic Programming

```python
#!/usr/bin/env python3
"""
Two-Stage Stochastic Programming Example
First stage: Make decisions before seeing random demand
Second stage: Adjust based on realized demand
"""

import gurobipy as gp
from gurobipy import GRB
import random

def solve_two_stage_stochastic():
    random.seed(42)
    
    # ===== Problem Data =====
    scenarios = ["low", "medium", "high"]
    probabilities = {"low": 0.2, "medium": 0.5, "high": 0.3}
    
    # Demand
    demand = {
        "low": {"P1": 50, "P2": 30},
        "medium": {"P1": 80, "P2": 60},
        "high": {"P1": 120, "P2": 90},
    }
    
    # First stage cost (procurement cost)
    first_stage_cost = {"P1": 10, "P2": 15}
    
    # Second stage cost (emergency procurement cost)
    second_stage_cost = {"P1": 20, "P2": 30}
    
    # ===== Create Model =====
    model = gp.Model("two_stage_stochastic")
    
    # ===== Decision Variables =====
    # First stage variables
    x = model.addVars(["P1", "P2"], vtype=GRB.CONTINUOUS, lb=0, name="x")
    
    # Second stage variables (per scenario)
    y = {}
    for s in scenarios:
        for p in ["P1", "P2"]:
            y[s, p] = model.addVar(lb=0, vtype=GRB.CONTINUOUS, name=f"y_{s}_{p}")
    
    # ===== Objective Function =====
    # Minimize expected total cost
    model.setObjective(
        gp.quicksum(first_stage_cost[p] * x[p] for p in ["P1", "P2"]) +
        gp.quicksum(
            probabilities[s] * second_stage_cost[p] * y[s, p]
            for s in scenarios for p in ["P1", "P2"]
        ),
        GRB.MINIMIZE
    )
    
    # ===== Constraints =====
    # Second stage demand satisfaction constraints
    for s in scenarios:
        for p in ["P1", "P2"]:
            model.addConstr(
                x[p] + y[s, p] >= demand[s][p],
                name=f"demand_{s}_{p}"
            )
    
    # ===== Solve =====
    model.optimize()
    
    # ===== Results =====
    print("=" * 60)
    print("Two-Stage Stochastic Programming Results")
    print("=" * 60)
    
    if model.status == GRB.OPTIMAL:
        print(f"\nExpected total cost: {model.objVal:.2f}")
        print("\nFirst stage decisions (procurement):")
        for p in ["P1", "P2"]:
            print(f"  {p}: {x[p].x:.1f} units")
        
        print("\nSecond stage decisions (emergency procurement):")
        for s in scenarios:
            print(f"  Scenario '{s}' (probability {probabilities[s]}):")
            for p in ["P1", "P2"]:
                print(f"    {p}: {y[s, p].x:.1f} units")
    
    return model

if __name__ == "__main__":
    solve_two_stage_stochastic()
```

---

## More Examples

More example code available at:
- `ormdsl-gurobi/examples/` - Gurobi integration examples
- `ormdsl-syn-dev/` - DSL syntax examples
