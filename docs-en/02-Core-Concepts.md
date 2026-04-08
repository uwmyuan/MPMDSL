# ORMDSL Core Concepts

This document details the core philosophy and key concepts of ORMDSL.

---

## 1. Constrained Set Class (CS Class)

### 1.1 What is a CS Class?

The Constrained Set Class (CS Class) is ORMDSL's fundamental building unit. It packages problem domain concepts with the constraints that concept must satisfy.

### 1.2 Analogy

| Traditional Approach | CS Class Approach |
|---------------------|------------------|
| Define variable `x` | Define class `Vehicle` |
| Define constraint `x <= capacity` | Define constraint in class `vehicle.capacity` |
| Variables and constraints are separated | Concepts and constraints are bound |

### 1.3 CS Class Components

```
CS Class contains:
├── Domain Type            # Basic type of the class
├── Fields                 # Parameters and decision variables
├── Constraints            # Embedded constraint definitions
└── Expression Methods      # Output constraint expressions
```

### 1.4 Code Example

```python
# Define a vessel constrained set class
class Vessel:
    # Decision variables
    arrival_time: DecisionVariable
    departure_time: DecisionVariable
    
    # Parameters
    length: Parameter
    deadline: Parameter
    
    # Embedded constraints
    def __init__(self, length, deadline):
        self.length = length
        self.deadline = deadline
        self.arrival_time = DecisionVariable(0, 1000)
        self.departure_time = DecisionVariable(0, 1000)
    
    # Constraint definitions
    def operational_constraints(self):
        return [
            # Departure time = Arrival time + Service time
            self.departure_time == self.arrival_time + self.length,
            # Must complete before deadline
            self.departure_time <= self.deadline
        ]

# Define berth constrained set class
class Berth:
    berth_no: Parameter
    available_time: Parameter
    
    def __init__(self, berth_no):
        self.berth_no = berth_no
        self.available_time = Parameter(0)
```

---

## 2. Decision Variables

### 2.1 Variable Types

| Type | Description | Python Representation | AMPL Representation |
|------|-------------|----------------------|---------------------|
| Continuous | Any real value | `GRB.CONTINUOUS` | `var` |
| Integer | Integer value | `GRB.INTEGER` | `var` |
| Binary | 0 or 1 | `GRB.BINARY` | `var` |

### 2.2 Variable Definition

```python
# Continuous variable (default)
x = DecisionVariable("x", lb=0, ub=100, vtype="continuous")

# Integer variable
n = DecisionVariable("n", lb=0, ub=1000, vtype="integer")

# Binary variable
y = DecisionVariable("y", lb=0, ub=1, vtype="binary")
# Or shorthand
y = BinaryVariable("y")
```

### 2.3 Indexed Variables

```python
# 1-dimensional indexed variable
x = IndexedVariable("x", index_set=customers)

# 2-dimensional indexed variable
x = IndexedVariable("x", indices=(customers, facilities))

# Usage example
model.addConstr(x["customer1", "facility1"] <= capacity)
```

---

## 3. Parameters

### 3.1 Parameter Definition

```python
# Scalar parameter
capacity = Parameter("capacity", value=100)

# Indexed parameter
distance = IndexedParameter("distance", 
                            indices=(customers, facilities),
                            values=distance_matrix)
```

### 3.2 Parameter vs Variable

| Property | Parameter | Decision Variable |
|----------|-----------|-------------------|
| Value | Known before solving | Determined by solver |
| Role | Input data | Optimization decision |
| Constraint | None | Has constraints |

---

## 4. Algebraic Expressions

### 4.1 Arithmetic Operators

```python
# Addition (+)
expr = x + y

# Subtraction (-)
expr = x - y

# Multiplication (*)
expr = 2 * x
expr = a * x + b * y  # Linear expression

# Division (/)
expr = x / 2

# Power operation
expr = x ** 2  # Quadratic term
```

### 4.2 Comparison Operators

```python
# Equal to (=)
expr = (x == y)

# Less than or equal to (<=)
expr = (x <= capacity)

# Greater than or equal to (>=)
expr = (x >= 0)

# Strict inequality (<, >)
expr = (x < capacity)
expr = (x > minimum)
```

### 4.3 Aggregation Operations

```python
# Sum (SUM)
total = Sum(i in customers, demand[i])

# Minimum (MIN)
min_demand = Min(i in customers, demand[i])

# Maximum (MAX)
max_demand = Max(i in customers, demand[i])

# Count (COUNT)
count = Count(i in customers, x[i] == 1)
```

---

## 5. Set Operations

### 5.1 Basic Sets

```python
# Create sets
I = Set("customers", elements=["c1", "c2", "c3", "c4"])
J = Set("facilities", elements=["f1", "f2", "f3"])

# Set size
n = Cardinality(I)  # |I|
```

### 5.2 Set Operations

```python
# Cartesian product
CROSS = I * J  # All (i, j) pairs

# Subset
SUBSET = Filter(I, lambda i: demand[i] > 10)

# Set difference
DIFF = I - excluded_set

# Set union
UNION = set1 + set2
```

### 5.3 Indexed Expressions

```python
# Use indices in constraints
CONSTRAINT sum(j in J, x[i,j]) == 1  # Each customer assigned to exactly one facility

# Conditional indexing
CONSTRAINT sum(j in J, if demand[j] > threshold then x[i,j]) <= k
```

---

## 6. Constraints

### 6.1 Constraint Types

#### Simple Constraints
```python
# Form: lhs <= rhs
c1 = Constraint("c1", x + y <= 10)

# Form: lhs == rhs
c2 = Constraint("c2", x == y)
```

#### Indexed Constraints
```python
# Generate constraint for each element in set
c = Constraint("assign", 
               sum(x[i,j] for j in J) == 1, 
               index=i in I)
```

#### Range Constraints
```python
# Form: lb <= expr <= ub
c = RangeConstraint("range", lb=5, expr=x+y, ub=15)
```

### 6.2 Constraint Naming

```python
# Good naming
c_capacity = Constraint("factory_capacity_limit", ...)

# Bad naming
c1 = Constraint(...)
```

### 6.3 Constraint Sets

```python
# Define a group of related constraints
class CapacityConstraints(ConstraintSet):
    def __init__(self, factories):
        self.constraints = []
        for f in factories:
            self.constraints.append(
                Constraint(f"capacity_{f}", 
                          production[f] <= max_capacity[f])
            )
```

---

## 7. Objective Function

### 7.1 Objective Types

```python
# Minimize (default)
model.setObjective(total_cost, sense="minimize")
model.setObjective(Min(total_cost), sense="minimize")

# Maximize
model.setObjective(total_profit, sense="maximize")
model.setObjective(Max(total_profit), sense="maximize")
```

### 7.2 Multi-Objective

```python
# Priority objectives
model.setMultiObjective([
    (primary_cost, priority=1),
    (secondary_time, priority=2)
])

# Weighted objectives
model.setObjective(0.7 * cost + 0.3 * time, sense="minimize")
```

### 7.3 Common Objective Function Forms

```python
# Linear objective: min sum(c[i] * x[i])
total_cost = Sum(i in I, c[i] * x[i])
model.setObjective(total_cost)

# Quadratic objective: min sum(sum(d[i,j] * x[i] * x[j]))
model.setObjective(QuadSum(d, x))

# Piecewise linear objective
model.setObjective(PiecewiseLinear(objective_expr, breakpoints))
```

---

## 8. Problem Modeling

### 8.1 Modeling Workflow

```
┌─────────────────────────────────────────────────────────┐
│  Step 1: Understand the Problem                         │
│          - Identify decision variables                  │
│          - Identify constraints                          │
│          - Identify objective function                   │
└─────────────────────┬───────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────┐
│  Step 2: Define CS Classes                              │
│          - Model problem domain concepts as classes      │
│          - Define variables and parameters in classes   │
│          - Define constraints in classes                 │
└─────────────────────┬───────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────┐
│  Step 3: Compose the Model                              │
│          - Instantiate CS classes                        │
│          - Add global constraints                        │
│          - Set objective function                        │
└─────────────────────┬───────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────┐
│  Step 4: Code Generation and Solving                    │
│          - Select target solver                         │
│          - Generate code                                 │
│          - Solve and analyze results                    │
└─────────────────────────────────────────────────────────┘
```

### 8.2 Example: Facility Location Problem

```python
# Step 1: Define CS Classes
class Customer:
    def __init__(self, name, demand):
        self.name = name
        self.demand = demand
        self.assignment = IndexedVariable("x", 
                                         index_set=facilities,
                                         vtype="binary")

class Facility:
    def __init__(self, name, capacity, opening_cost):
        self.name = name
        self.capacity = capacity
        self.opening_cost = opening_cost
        self.is_open = BinaryVariable(f"y_{name}")

# Step 2: Instantiate
customers = [Customer(f"c{i}", demand[i]) for i in range(5)]
facilities = [Facility(f"f{j}", cap[j], cost[j]) for j in range(4)]

# Step 3: Compose model
model = Model("facility_location")

# Add constraints
# Each customer assigned to exactly one facility
for c in customers:
    model.addConstraint(sum(c.assignment[f] for f in facilities) == 1)

# Customers can only be assigned to open facilities
for c in customers:
    for f in facilities:
        model.addConstraint(c.assignment[f] <= f.is_open)

# Facility capacity constraints
for f in facilities:
    model.addConstraint(
        sum(c.demand * c.assignment[f] for c in customers) <= f.capacity
    )

# Step 4: Set objective function
opening_cost = Sum(f in facilities, f.opening_cost * f.is_open)
assignment_cost = Sum(c in customers, f in facilities, 
                       distance[c,f] * c.assignment[f])
model.setObjective(opening_cost + assignment_cost)

# Step 5: Generate code and solve
code = model.toGurobi()
model.solve()
```

---

## 9. Advanced Features

### 9.1 Implication Constraints

```python
# if x == 1 then y == 1
# Auto-converted to Big-M constraint: y >= x
constraint = Implies(x == 1, y == 1)
```

### 9.2 Disjunction Constraints

```python
# x == a OR x == b
constraint = Disjunction([
    (x == a, "case_a"),
    (x == b, "case_b")
])
```

### 9.3 Conditional Constraints

```python
# if condition then constraint
constraint = IfThen(
    condition=(i in selected_set),
    then=(x[i] <= capacity)
)
```

### 9.4 Symmetry Breaking

```python
# Auto-add symmetry breaking constraints
model.addSymmetryBreaking(
    variables=y, 
    ordering="lexicographic"
)
```

---

## 10. Best Practices

### 10.1 Naming Conventions

```python
# Use meaningful names
capacity_constraint = Constraint("max_capacity", ...)

# Avoid abbreviations
# ❌ c1, c2, var_x
# ✅ capacity_constraint, decision_variable_x
```

### 10.2 Modularity

```python
# Encapsulate related constraints as functions
def create_vehicle_constraints(vehicles, berths):
    constraints = []
    for v in vehicles:
        constraints.append(...)
    return constraints

# Reuse in model
model.addConstraints(create_vehicle_constraints(v, b))
```

### 10.3 Comments

```python
# Add explanatory comments
"""
Capacity constraint: Ensure each factory's output does not exceed maximum capacity
Formula: sum(production[f,i] for i in products) <= max_capacity[f]
"""
capacity_constraint = Constraint(...)
```
