# ORMDSL Best Practices

This document provides performance optimization and coding standards guidelines.

---

## 1. Model Construction Best Practices

### 1.1 Variable Definition

#### ✅ Recommended

```python
# Pre-calculate index list
indices = [f"item_{i}" for i in range(n)]

# Use meaningful variable names
production = model.addVars(products, lb=0, name="production")
demand = model.addVars(customers, lb=0, name="demand")

# Set reasonable variable bounds
x = model.addVar(lb=0, ub=max_capacity, name="x")  # Avoid unbounded variables
```

#### ❌ Avoid

```python
# Don't use unbounded variables (may lead to unbounded solutions)
x = model.addVar(vtype=GRB.CONTINUOUS)  # Dangerous!

# Avoid numeric indices
x1, x2, x3 = model.addVar(), model.addVar(), model.addVar()

# Don't use reserved words as variable names
model.addVar(name="for")  # Error!
```

### 1.2 Constraint Writing

#### ✅ Recommended

```python
# Decompose complex constraints
raw_materials = gp.quicksum(consumption[p, r] * x[p] for p in products)
model.addConstr(raw_materials <= available[r], name="material_limit")

# Name constraints
model.addConstr(x + y <= capacity, name="capacity_constraint")

# Use auxiliary variables to simplify complex expressions
z = model.addVar(name="z")
model.addConstr(z == x * y)  # Linearize later
```

#### ❌ Avoid

```python
# Avoid overly large constraint expressions
model.addConstr(
    sum(a[i] * b[i] * c[i] * x[i] 
        for i in range(10000)) <= M  # May be slow
)

# Don't skip constraint naming (hard to debug)
model.addConstr(x + y <= 10)  # Unnamed constraint
```

---

## 2. Performance Optimization

### 2.1 Pre-solve and Data Preprocessing

```python
# Filter impossible combinations
feasible_pairs = [
    (i, j) for i in range(n) for j in range(m)
    if distance[i, j] <= max_distance
]

x = model.addVars(feasible_pairs, vtype=GRB.BINARY)

# Pre-calculate constants
total_fixed_cost = sum(fixed_cost[j] for j in facilities)
```

### 2.2 Use Appropriate Variable Types

```python
# If variable only takes 0/1, use binary variables
y = model.addVar(vtype=GRB.BINARY)  # Faster than CONTINUOUS + extra constraints

# If integer variable range is small, use INTEGER
shift_count = model.addVar(vtype=GRB.INTEGER, lb=0, ub=5)
```

### 2.3 Constraint Linearization

```python
# Quadratic constraint linearization (Big-M method)
# Original: z = x * y (x, y, z are all binary)
# Linearize:
model.addConstr(z <= x)
model.addConstr(z <= y)
model.addConstr(z >= x + y - 1)

# Absolute value constraint
# Original: |expr| <= M
# Linearize:
model.addConstr(expr <= M)
model.addConstr(-expr <= M)
```

### 2.4 Symmetry Breaking

```python
# Bin ordering (avoid equivalent solutions)
for k in range(n_bins - 1):
    model.addConstr(y[k] >= y[k + 1])

# Customer assignment ordering
for j in range(n_facilities - 1):
    model.addConstr(
        gp.quicksum(x[i, j] for i in customers) >=
        gp.quicksum(x[i, j + 1] for i in customers)
    )
```

### 2.5 Solver Parameter Tuning

```python
# Set time limit
model.Params.TimeLimit = 3600  # 1 hour

# Set optimality gap tolerance
model.Params.MIPGap = 0.001  # 0.1%

# Adjust presolve level
model.Params.Presolve = 2  # 0=off, 1=conservative, 2=aggressive

# Adjust thread count (based on CPU cores)
model.Params.Threads = 8

# Adjust cut plane strength
model.Params.Cuts = 2  # 0=off, 1=conservative, 2=aggressive, 3=very aggressive

# Enable heuristic solving
model.Params.Heuristics = 0.5  # Between 0-1, higher is more aggressive
```

### 2.6 Large-Scale Problem Decomposition

```python
# Benders decomposition example framework
# Master problem: decide key variables
y = model.addVars(facilities, vtype=GRB.BINARY)

# Subproblem: given y, solve for x (multiple calls)
def solve_subproblem(y_values):
    submodel = gp.Model("subproblem")
    x = submodel.addVars(customers, facilities, vtype=GRB.CONTINUOUS)
    # ... subproblem constraints ...
    submodel.optimize()
    return submodel.objVal, submodel.getVars()

# Iterate and add Benders cuts
for iteration in range(max_iterations):
    # Solve master problem
    model.optimize()
    
    # Solve subproblem
    cost, solution = solve_subproblem(y)
    
    # Add cut
    if cost > master.objVal + tolerance:
        add_benders_cut(model, y, cost, solution)
```

---

## 3. Code Organization and Structure

### 3.1 Modular Modeling

```python
# ===== models/production.py =====

class ProductionModel:
    """Production planning model"""
    
    def __init__(self, name: str):
        self.model = gp.Model(name)
        self._vars = {}
        self._constrs = {}
    
    def add_decision_variables(self, products: List[str]):
        """Add decision variables"""
        self._vars['production'] = self.model.addVars(
            products, vtype=GRB.CONTINUOUS, lb=0, name="production"
        )
    
    def add_constraints(self, data: dict):
        """Add constraints"""
        # ...
    
    def set_objective(self, profit: dict):
        """Set objective function"""
        self._vars['profit'] = self.model.setObjective(
            gp.quicksum(profit[p] * self._vars['production'][p] 
                        for p in self._vars['production']),
            GRB.MAXIMIZE
        )
    
    def solve(self):
        """Solve model"""
        self.model.optimize()
        return self.model
    
    def get_results(self):
        """Extract results"""
        return {p: self._vars['production'][p].x 
                for p in self._vars['production']}


# ===== main.py =====
from models.production import ProductionModel

model = ProductionModel("monthly_production")
model.add_decision_variables(products)
model.add_constraints(data)
model.set_objective(profit)
result = model.solve()
```

### 3.2 Data Classes

```python
from dataclasses import dataclass
from typing import List

@dataclass
class Facility:
    """Facility data"""
    name: str
    capacity: float
    fixed_cost: float
    location: tuple
    
    def __hash__(self):
        return hash(self.name)

@dataclass
class Customer:
    """Customer data"""
    name: str
    demand: float
    location: tuple

# Usage
facilities = [Facility(f"F{i}", cap[i], cost[i], loc[i]) for i in range(n)]
customers = [Customer(f"C{i}", dem[i], loc[i]) for i in range(m)]
```

### 3.3 Configuration Management

```python
# ===== config.py =====

class SolverConfig:
    """Solver configuration"""
    
    def __init__(self):
        self.time_limit = 3600
        self.mip_gap = 0.001
        self.threads = 8
        self.presolve = 2
    
    def apply(self, model: gp.Model):
        """Apply configuration to model"""
        model.Params.TimeLimit = self.time_limit
        model.Params.MIPGap = self.mip_gap
        model.Params.Threads = self.threads
        model.Params.Presolve = self.presolve

# Usage
config = SolverConfig()
config.time_limit = 7200  # Custom time limit
config.apply(model)
```

---

## 4. Debugging and Validation

### 4.1 Model Validation

```python
def validate_model(model: gp.Model) -> bool:
    """Validate model completeness"""
    
    # Check number of variables
    num_vars = len(model.getVars())
    print(f"Number of variables: {num_vars}")
    
    # Check number of constraints
    num_constrs = len(model.getConstrs())
    print(f"Number of constraints: {num_constrs}")
    
    # Check for unbounded variables
    for var in model.getVars():
        if var.ub > 1e9 and var.lb < -1e9:
            print(f"Warning: Variable {var.varName} may be unbounded")
    
    # Check for conflicting constraints
    try:
        model.optimize()
        return True
    except Exception as e:
        print(f"Model error: {e}")
        return False
```

### 4.2 Constraint Sensitivity Analysis

```python
# Analyze impact of RHS changes
model.optimize()
base_obj = model.objVal

for c in model.getConstrs():
    # Temporarily modify RHS
    old_rhs = model.getConstrByName(c.constrName).rhs
    model.getConstrByName(c.constrName).rhs += 1
    model.optimize()
    
    if model.status == GRB.OPTIMAL:
        shadow_price = model.objVal - base_obj
        print(f"{c.constrName}: shadow price = {shadow_price}")
    
    # Restore RHS
    model.getConstrByName(c.constrName).rhs = old_rhs
```

---

## 5. Code Style Guide

### 5.1 Naming Conventions

```python
# Variable names
production_rate = 10      # lowercase with underscore
customer_demand = 50      # lowercase with underscore
MAX_CAPACITY = 100       # uppercase with underscore (constant)

# Function names
def calculate_total_cost(): pass
def solve_inventory_problem(): pass

# Class names
class ProductionScheduler(): pass
class RouteOptimizer(): pass

# Model names
model = gp.Model("inventory_optimization")
```

### 5.2 Comment Standards

```python
# Module documentation
"""
Production Planning Optimization Model

This module implements a mixed integer programming model for determining optimal production plans.
Reference: Smith & Jones (2020), "Production Planning Models"
"""

def solve_production():
    # ===== Problem Data =====
    # Product list
    products = ["P1", "P2", "P3"]
    
    # Unit profit ($/unit)
    profit = {"P1": 10, "P2": 15, "P3": 12}
    
    # ===== Model Construction =====
    model = gp.Model("production")
    
    # ===== Decision Variables =====
    # x[p] = production quantity of product p
    x = model.addVars(products, vtype=GRB.CONTINUOUS, lb=0)
```

---

## 6. Testing Best Practices

### 6.1 Unit Testing

```python
import unittest

class TestKnapsackModel(unittest.TestCase):
    """Knapsack model tests"""
    
    def setUp(self):
        """Setup before test"""
        self.model = create_knapsack_model(sample_data)
    
    def test_solution_exists(self):
        """Test solution existence"""
        self.model.optimize()
        self.assertEqual(self.model.status, GRB.OPTIMAL)
    
    def test_solution_feasibility(self):
        """Test solution feasibility"""
        self.model.optimize()
        x = self.model.getVars()
        total_weight = sum(w[i] * x[i] for i in items)
        self.assertLessEqual(total_weight, capacity)
    
    def test_objective_bounds(self):
        """Test objective bounds"""
        self.model.optimize()
        # Optimal solution should be >= greedy solution
        greedy = solve_greedy(sample_data)
        self.assertGreaterEqual(self.model.objVal, greedy)

if __name__ == "__main__":
    unittest.main()
```

### 6.2 Performance Benchmarking

```python
import time
from functools import wraps

def benchmark(func):
    """Performance benchmark decorator"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        elapsed = time.time() - start
        print(f"{func.__name__}: {elapsed:.4f}s")
        return result
    return wrapper

@benchmark
def solve_large_instance(model):
    model.optimize()
    return model.objVal
```

---

## 7. Production Deployment

### 7.1 Environment Configuration

```yaml
# requirements.txt
gurobipy==10.0.0
pandas>=1.5.0
numpy>=1.24.0
python-dotenv>=1.0.0

# .env
GUROBI_LICENSE_FILE=/path/to/gurobi.lic
SOLVER_TIME_LIMIT=3600
SOLVER_MIP_GAP=0.001
```

### 7.2 Logging

```python
import logging
import sys

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('optimization.log'),
        logging.StreamHandler(sys.stdout)
    ]
)

logger = logging.getLogger(__name__)

def solve_model(data):
    logger.info(f"Starting model solve: {data['instance_name']}")
    logger.info(f"Variables: {n_vars}, Constraints: {n_constrs}")
    
    model.optimize()
    
    if model.status == GRB.OPTIMAL:
        logger.info(f"Optimal solution: {model.objVal}")
        logger.info(f"Solve time: {model.runtime:.2f}s")
    else:
        logger.warning(f"Solve status: {model.status}")
    
    return model
```
