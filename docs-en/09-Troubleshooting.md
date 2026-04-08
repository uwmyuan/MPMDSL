# ORMDSL Troubleshooting Guide

This document helps solve common problems encountered when using ORMDSL.

---

## 1. Installation Issues

### 1.1 Gurobi License Issues

#### Problem: `LicenseError` or `GRBException`

```
gurobipy.gurobipy.GRBException: License error
```

#### Solution

```bash
# 1. Check license file
cat ~/.gurobi/gurobi.lic

# 2. Reacquire license
grbgetkey <your-license-key>

# 3. Set license path
export GRB_LICENSE_FILE=/path/to/gurobi.lic  # Linux/macOS
set GRB_LICENSE_FILE=C:\path\to\gurobi.lic  # Windows

# 4. Verify academic license
python -c "import gurobipy; print(gp.gurobi.version())"
```

### 1.2 Python Version Incompatibility

#### Problem: `SyntaxError` or module import failure

```
ModuleNotFoundError: No module named 'gurobipy'
```

#### Solution

```bash
# Check Python version
python --version

# Ensure using correct Python
which python
which pip

# Reinstall
pip uninstall gurobipy
pip install gurobipy

# Use virtual environment
python -m venv venv
source venv/bin/activate  # Linux/macOS
# or
venv\Scripts\activate  # Windows
pip install gurobipy
```

---

## 2. Model Construction Issues

### 2.1 Model Infeasible

#### Problem: `GRB.OPTIMAL` status is `GRB.INFEASIBLE`

```
Optimization ended with status: 3
```

#### Diagnostic Steps

```python
import gurobipy as gp

model.optimize()

if model.status == GRB.INFEASIBLE:
    # Compute infeasible constraints
    model.computeIIS()
    
    print("Infeasible constraints:")
    for c in model.getConstrs():
        if c.iisconstr:
            print(f"  - {c.constrName}")
    
    print("\nInfeasible variables:")
    for v in model.getVars():
        if v.iisub:
            print(f"  - {v.varName}")
```

#### Solutions

1. **Check constraint correctness**
```python
# Wrong constraint
model.addConstr(x >= 10)  # Should be x <= 10
model.addConstr(x + y >= 100)  # Should be <=

# Check data
print(f"x range: [{x.lb}, {x.ub}]")
print(f"y range: [{y.lb}, {y.ub}]")
```

2. **Relax constraints to find conflicts**
```python
# Add artificial variables (penalty terms)
model.setParam("IntFeasTol", 1e-6)
model.setParam("FeasibilityTol", 1e-6)

# Or use constraint relaxation
def add_relaxed_constraints(model):
    for c in model.getConstrs():
        c.slack  # Check slack
```

3. **Common infeasibility causes**
- Conflicting constraints: `x >= 10` and `x <= 5`
- Integer constraints too tight
- Input data errors

### 2.2 Model Unbounded

#### Problem: `GRB.UNBOUNDED`

```
Optimization ended with status: 5
```

#### Diagnostic Steps

```python
if model.status == GRB.UNBOUNDED:
    # Check unbounded variables
    model.computeIIS()
    
    print("Unbounded variables:")
    for v in model.getVars():
        if v.iisub:
            print(f"  - {v.varName}")
```

#### Solutions

1. **Set variable bounds**
```python
# Add reasonable lower and upper bounds
x = model.addVar(lb=0, ub=10000, vtype=GRB.CONTINUOUS)
```

2. **Check objective function**
```python
# Ensure objective direction is correct
model.setObjective(cost, GRB.MINIMIZE)  # Cannot minimize unbounded direction
```

### 2.3 Numerical Issues

#### Problem: `GRB.NUMERIC` or unstable solutions

```
Warning: numeric difficulties
```

#### Solutions

```python
# Adjust numerical parameters
model.Params.NumericFocus = 3  # Improve numerical stability
model.Params.ScaleFlag = 2     # More aggressive scaling

# Avoid large coefficient differences
# Constraint: 1000000*x <= 1 changed to: x <= 0.000001

# Normalize data
for i in range(n):
    normalized_data[i] = original_data[i] / max_value
```

---

## 3. Solve Performance Issues

### 3.1 Solve Time Too Long

#### Problem: Model runs beyond expected time

#### Diagnostic Steps

```python
# Check model scale
print(f"Variables: {len(model.getVars())}")
print(f"Constraints: {len(model.getConstrs())}")
print(f"Non-zeros: {model.NumNZs}")

# Check LP relaxation
model.optimize()
if model.status == GRB.TIME_LIMIT:
    print(f"Current objective: {model.objVal}")
    print(f"Optimal bound: {model.objBound}")
    print(f"Gap: {100 * model.MIPGap}%")
```

#### Solutions

1. **Set time limit**
```python
model.Params.TimeLimit = 300  # 5 minutes
```

2. **Adjust solve strategy**
```python
# Use dual simplex
model.Params.Method = 1

# Adjust cut planes
model.Params.Cuts = 1

# Enable heuristics
model.Params.Heuristics = 0.8
```

3. **Simplify model**
```python
# Presolve
model.Params.Presolve = 2

# Aggregate similar constraints
model.feasRelax(1, True, False, True)
```

### 3.2 Optimality Gap Too Large

#### Problem: Solver stops early with large gap

```
MIPGap: 10%
```

#### Solutions

```python
# Tighten gap tolerance
model.Params.MIPGap = 0.001  # 0.1%

# Increase solve time
model.Params.TimeLimit = 7200

# Adjust node selection strategy
model.Params.NodeFileStart = 3  # Write to disk earlier
model.Params.NodeMethod = 2     # Use dual simplex
```

---

## 4. Code Errors

### 4.1 Common Python Errors

#### Index Errors

```python
# Wrong
for i in range(n):
    for j in range(m):
        cost[i, j] = data[i][j]  # Assuming data is a list

# Correct
for i in range(n):
    for j in range(m):
        cost[i, j] = data.iloc[i, j]  # Use pandas DataFrame
```

#### Variable Scope Errors

```python
# Wrong
def create_model():
    x = model.addVar()  # x is local variable
    return model

x_solution = model.getVarByName("x")  # Cannot access outside function

# Correct
def create_model():
    model = gp.Model()
    model._x = model.addVar()  # Save reference
    return model

model = create_model()
x = model.getVarByName("x")
```

### 4.2 Constraint Definition Errors

#### Both sides of constraint are variables

```python
# Wrong
model.addConstr(x, GRB.EQUAL, y)  # x == y syntax error

# Correct
model.addConstr(x == y)
model.addConstr(x - y == 0)
model.addConstr(x >= y)
```

#### Wrong set comprehension

```python
# Wrong
model.addConstrs(x[i] for i in items)  # Need index

# Correct
for i in items:
    model.addConstr(x[i] <= capacity)
    
# Or
model.addConstrs(x[i] <= capacity[i] for i in items)
```

---

## 5. Solve Result Issues

### 5.1 Solution Doesn't Match Expectations

#### Diagnostic Steps

```python
model.optimize()

if model.status == GRB.OPTIMAL:
    print("Variable values:")
    for v in model.getVars():
        if v.x > 0:
            print(f"  {v.varName} = {v.x}")
    
    print("\nConstraint check:")
    for c in model.getConstrs():
        lhs = model.getRow(c)
        slack = c.slack
        if slack < -1e-6:  # Constraint violated
            print(f"  Constraint violated: {c.constrName} (slack={slack})")
```

#### Common Causes

1. **Constraint definition error**
```python
# Should be <= but written as >=
model.addConstr(x + y <= 10)  # Check if this is intended
```

2. **Data error**
```python
# Check input data
print(cost_matrix)
print(capacity_values)
```

3. **Wrong variable type**
```python
# Should be integer but set as continuous
x = model.addVar(vtype=GRB.CONTINUOUS)  # Should be INTEGER
```

### 5.2 Negative Objective Value

#### Diagnostic

```python
if model.objVal < 0:
    print("Objective value is negative!")
    print("Check objective definition")
    
    # Print objective expression
    print(model.getObjective())
```

#### Solutions

```python
# If minimizing cost and cost should be positive
# Ensure cost coefficients are correct
model.setObjective(
    gp.quicksum(cost[i] * x[i] for i in items),
    GRB.MINIMIZE
)

# If using penalty terms, ensure sign is correct
# Penalty: + M * violation
```

---

## 6. Advanced Issues

### 6.1 Large Model Memory Issues

```python
# Use sparse matrix
model.params.SparseSol = 1

# Write to disk to free memory
model.params.NodeFileStart = 3  # Store nodes on disk

# Process in chunks
chunk_size = 1000
for chunk in range(0, n, chunk_size):
    # Process current chunk
    process_chunk(data[chunk:chunk+chunk_size])
```

### 6.2 Parallel Solve Conflicts

```python
# Limit thread count
model.Params.Threads = 4

# Or disable parallel
model.Params.Method = 0  # Simplex (serial)
```

### 6.3 Multi-Objective Solving

```python
# Priority method
model.setObjectiveN(obj1, index=0, priority=2, name="primary")
model.setObjectiveN(obj2, index=1, priority=1, name="secondary")

# Weighted sum method
model.setObjective(obj1 + lambda * obj2, GRB.MINIMIZE)
```

---

## 7. Getting Help

### 7.1 Debug Checklist

```python
# Complete debug checklist
def debug_model(model):
    """Model debug function"""
    results = {
        "status": model.status,
        "vars": len(model.getVars()),
        "constrs": len(model.getConstrs()),
        "nz": model.NumNZs,
    }
    
    if model.status == GRB.OPTIMAL:
        results["objval"] = model.objVal
        results["runtime"] = model.runtime
        results["gap"] = model.MIPGap
    elif model.status == GRB.INFEASIBLE:
        model.computeIIS()
        results["iis_constraints"] = [c.constrName for c in model.getConstrs() if c.iisconstr]
    elif model.status == GRB.UNBOUNDED:
        model.computeIIS()
        results["iis_vars"] = [v.varName for v in model.getVars() if v.iisub]
    
    return results
```

### 7.2 Contact Support

- See [Gurobi Documentation](https://www.gurobi.com/documentation/)
- See [Pyomo Documentation](http://www.pyomo.org/documentation/)
- Submit GitHub Issue

---

## 8. Error Code Reference

| Status Code | Name | Description |
|-------------|------|-------------|
| 1 | LOADED | Model loaded |
| 2 | OPTIMAL | Optimal solution |
| 3 | INFEASIBLE | Infeasible |
| 4 | INF_OR_UNBD | Infeasible or unbounded |
| 5 | UNBOUNDED | Unbounded |
| 6 | CUTOFF | Cut off |
| 7 | ITERATION_LIMIT | Iteration limit reached |
| 8 | NODE_LIMIT | Node limit reached |
| 9 | TIME_LIMIT | Time limit reached |
| 10 | SOLUTION_LIMIT | Solution limit reached |
| 11 | INTERRUPTED | Interrupted |
| 12 | NUMERIC | Numerical issue |
| 13 | SUBOPTIMAL | Suboptimal solution |
