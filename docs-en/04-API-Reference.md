# ORMDSL API Reference

This document provides the complete API reference for ORMDSL.

---

## 1. Core Classes

### 1.1 Model Class

```python
class Model:
    """Main optimization model class"""
```

#### Constructor

```python
Model(name: str)
```

| Parameter | Type | Description |
|-----------|------|-------------|
| name | str | Model name |

#### Methods

##### addVar()

Add a single decision variable.

```python
def addVar(self,
           name: str,
           vtype: str = "continuous",
           lb: float = 0.0,
           ub: float = float('inf'),
           obj: float = 0.0) -> Var
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| name | str | - | Variable name |
| vtype | str | "continuous" | Variable type: "continuous", "integer", "binary" |
| lb | float | 0.0 | Lower bound |
| ub | float | inf | Upper bound |
| obj | float | 0.0 | Objective coefficient |

##### addVars()

Add indexed variables.

```python
def addVars(self,
            indices: List[str],
            vtype: str = "continuous",
            lb: float = 0.0,
            ub: float = float('inf')) -> Dict[str, Var]
```

##### setObjective()

Set objective function.

```python
def setObjective(self,
                 expr: Expr,
                 sense: str = "minimize") -> None
```

| Parameter | Type | Description |
|-----------|------|-------------|
| expr | Expr | Objective expression |
| sense | str | "minimize" or "maximize" |

##### addConstr()

Add a constraint.

```python
def addConstr(self,
              name: str,
              constraint: Expr,
              index: str = None) -> Constraint
```

##### solve()

Solve the model.

```python
def solve(self,
          solver: str = "gurobi",
          time_limit: float = None,
          mip_gap: float = None) -> SolveResult
```

| Parameter | Type | Description |
|-----------|------|-------------|
| solver | str | Solver: "gurobi", "cplex", "glpk" |
| time_limit | float | Time limit in seconds |
| mip_gap | float | Optimality gap tolerance |

##### toAMPL()

Generate AMPL code.

```python
def toAMPL(self) -> str
```

##### toGurobi()

Generate Gurobi Python code.

```python
def toGurobi(self) -> str
```

##### toLatex()

Generate LaTeX formulas.

```python
def toLatex(self) -> str
```

---

### 1.2 Var Class

Decision variable class.

```python
class Var:
    """Decision variable"""
    
    def __init__(self, name: str, vtype: str, lb: float, ub: float)
    
    name: str      # Variable name
    vtype: str     # Variable type
    lb: float      # Lower bound
    ub: float      # Upper bound
    value: float   # Value after solving
```

#### Operator Overloading

```python
# Arithmetic operations
var + var     # Addition
var - var     # Subtraction
var * var     # Multiplication
var / var     # Division
var ** n      # Power

# Comparison operations
var <= expr   # Less than or equal
var >= expr   # Greater than or equal
var == expr   # Equal
```

---

### 1.3 Expr Class

Algebraic expression class.

```python
class Expr:
    """Algebraic expression"""
    
    def __init__(self)
    
    @staticmethod
    def Constant(value: float) -> Expr
    
    @staticmethod
    def Var(v: Var) -> Expr
    
    @staticmethod
    def Sum(terms: List[Expr]) -> Expr
    
    @staticmethod
    def LinExpr(coeffs: List[float], vars: List[Var]) -> Expr
```

---

## 2. Constraints

### 2.1 Constraint Class

```python
class Constraint:
    """Base constraint class"""
    
    def __init__(self, name: str, lhs: Expr, sense: str, rhs: Expr)
    
    name: str    # Constraint name
    lhs: Expr    # Left-hand side expression
    sense: str   # Relation: "<=", ">=", "=="
    rhs: Expr    # Right-hand side expression
```

### 2.2 RangeConstraint Class

Range constraint.

```python
class RangeConstraint:
    """Range constraint: lb <= expr <= ub"""
    
    def __init__(self,
                 name: str,
                 expr: Expr,
                 lb: float,
                 ub: float)
```

### 2.3 QualifiedConstraint Class

Constraint with index qualification.

```python
class QualifiedConstraint:
    """Constraint with qualification"""
    
    def __init__(self,
                 name: str,
                 constraint: Constraint,
                 index_set: List[str],
                 filter_func: Callable = None)
```

### 2.4 IndicatorConstraint Class

Indicator constraint.

```python
class IndicatorConstraint:
    """Indicator constraint: if x == 1 then expr <= rhs"""
    
    def __init__(self,
                 indicator_var: Var,
                 indicator_value: int,
                 expr: Expr,
                 sense: str,
                 rhs: Expr)
```

---

## 3. Set Operations

### 3.1 Set Class

```python
class Set:
    """Index set"""
    
    def __init__(self, name: str, elements: List[Any])
    
    name: str           # Set name
    elements: List[Any] # Set elements
    size: int           # Set size
    
    def __iter__(self)
    def __len__(self)
    def __contains__(self, item)
```

### 3.2 SetExpr Class

Set expression.

```python
class SetExpr:
    """Set expression"""
    
    @staticmethod
    def Union(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Intersection(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Difference(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Cross(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Filter(set: Set, predicate: Callable) -> Set
```

---

## 4. Aggregation Functions

### 4.1 Sum Function

Summation aggregation.

```python
def Sum(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr
```

**Example:**

```python
# sum(i in customers, demand[i] * x[i])
Sum(customers, lambda i: demand[i] * x[i])
```

### 4.2 Min/Max Functions

Min/Max aggregation.

```python
def Min(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr

def Max(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr
```

### 4.3 Count Function

Count aggregation.

```python
def Count(index_set: Set,
          predicate: Callable[[Any], bool]) -> Expr
```

**Example:**

```python
# count(i in I, x[i] == 1)
Count(customers, lambda i: x[i] == 1)
```

### 4.4 Cardinality Function

Set size.

```python
def Cardinality(set: Set) -> Expr
```

---

## 5. Constrained Set Class (CS Class)

### 5.1 CS Base Class

```python
class CS:
    """Constrained Set Class base class"""
    
    def __init__(self):
        self.variables = []
        self.parameters = []
        self.constraints = []
    
    def get_constraints(self) -> List[Constraint]
    def get_variables(self) -> List[Var]
    def get_parameters(self) -> List[Parameter]
```

### 5.2 Usage Example

```python
class Vehicle(CS):
    """Vessel constrained set class"""
    
    def __init__(self, name: str, length: float, service_time: float):
        super().__init__()
        self.name = name
        self.length = length
        self.service_time = service_time
        
        # Decision variables
        self.arrival = Var(f"{name}_arr", lb=0)
        self.departure = Var(f"{name}_dep", lb=0)
        
        self.variables = [self.arrival, self.departure]
    
    def get_constraints(self) -> List[Constraint]:
        return [
            Constraint(f"{self.name}_service",
                       self.departure - self.arrival,
                       "==",
                       self.service_time)
        ]
```

---

## 6. Result Classes

### 6.1 SolveResult Class

Solve result.

```python
class SolveResult:
    """Solve result"""
    
    status: str           # Solve status: "OPTIMAL", "TIME_LIMIT", etc.
    objective_value: float  # Objective value
    runtime: float         # Solve time in seconds
    mip_gap: float         # Final optimality gap
    num_vars: int          # Number of variables
    num_constraints: int    # Number of constraints
    
    def is_optimal(self) -> bool
    def is_feasible(self) -> bool
    def get_value(self, var: Var) -> float
    def get_values(self, vars: List[Var]) -> Dict[str, float]
```

---

## 7. Code Generation

### 7.1 AMPL Generation

```python
from ormdsl import Model

model = Model("example")
# ... define model ...

ampl_code = model.toAMPL()

# Save to file
with open("model.run", "w") as f:
    f.write(ampl_code)
```

**Generated AMPL code format:**

```ampl
# Model: example
# Generated by ORMDSL

# Decision Variables
var x >= 0, <= 10;

# Objective
minimize obj: 10 * x;

# Constraints
subject to c1: 2 * x <= 15;
```

### 7.2 Gurobi Generation

```python
gurobi_code = model.toGurobi()

# Save to file
with open("model.py", "w") as f:
    f.write(gurobi_code)
```

**Generated Gurobi code format:**

```python
#!/usr/bin/env python3
import gurobipy as gp
from gurobipy import GRB

def create_model():
    model = gp.Model("example")
    x = model.addVar(lb=0, ub=10, vtype=GRB.CONTINUOUS)
    model.setObjective(10 * x, GRB.MINIMIZE)
    model.addConstr(2 * x <= 15)
    model.optimize()
    return model

if __name__ == "__main__":
    create_model()
```

### 7.3 LaTeX Generation

```python
latex_code = model.toLatex()

# Output
print(latex_code)
```

**Generated LaTeX code:**

```latex
\begin{align}
\min \quad & 10x \\
\text{s.t.} \quad & 2x \leq 15 \\
             & x \geq 0
\end{align}
```

---

## 8. Configuration Parameters

### 8.1 Model Parameters

```python
model.params.time_limit = 3600        # Time limit in seconds
model.params.mip_gap = 0.001          # Optimality gap
model.params.threads = 8              # Number of threads
model.params.heuristics = 0.5          # Heuristic intensity
```

### 8.2 Solver Parameters

```python
model.solver_params["Gurobi"]["Method"] = 2       # Solve method
model.solver_params["Gurobi"]["Presolve"] = 2     # Presolve level
model.solver_params["CPLEX"]["TimeLimit"] = 3600  # CPLEX time limit
```

---

## 9. Exception Classes

### 9.1 ORMDSLException

Base exception class.

```python
class ORMDSLException(Exception):
    """ORMDSL base exception"""
    pass
```

### 9.2 Derived Exceptions

```python
class ModelError(ORMDSLException):
    """Model definition error"""
    pass

class ConstraintError(ORMDSLException):
    """Constraint definition error"""
    pass

class SolverError(ORMDSLException):
    """Solver error"""
    pass

class ValidationError(ORMDSLException):
    """Validation error"""
    pass
```

---

## 10. Constants

### 10.1 Variable Types

```python
VarType.CONTINUOUS = "continuous"  # Continuous variable
VarType.INTEGER = "integer"       # Integer variable
VarType.BINARY = "binary"         # Binary variable
```

### 10.2 Objective Direction

```python
Sense.MINIMIZE = "minimize"  # Minimize
Sense.MAXIMIZE = "maximize"  # Maximize
```

### 10.3 Solve Status

```python
Status.OPTIMAL = "OPTIMAL"           # Optimal solution
Status.FEASIBLE = "FEASIBLE"        # Feasible solution
Status.INFEASIBLE = "INFEASIBLE"    # Infeasible
Status.UNBOUNDED = "UNBOUNDED"      # Unbounded
Status.TIME_LIMIT = "TIME_LIMIT"    # Time limit reached
Status.MEMORY_LIMIT = "MEMORY_LIMIT" # Memory limit reached
```
