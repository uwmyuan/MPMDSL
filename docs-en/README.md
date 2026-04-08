# ORMDSL User Documentation

> **ORMDSL (Object-Relational Modeling for Discrete Optimization Domain Specific Language)**
>
> A domain-specific language for mathematical programming modeling, achieving natural mapping between problem domain concepts and optimization models through Constrained Set Classes.

---

## Table of Contents

| Document | Description |
|----------|-------------|
| [01-Overview.md](01-Overview.md) | Project introduction, design philosophy, core features |
| [02-Core-Concepts.md](02-Core-Concepts.md) | Constrained Set Classes, algebraic expressions, objective functions |
| [03-Quick-Start.md](03-Quick-Start.md) | 5-minute quick start guide |
| [04-API-Reference.md](04-API-Reference.md) | Complete API documentation |
| [05-Problem-Modeling.md](05-Problem-Modeling.md) | How to model various optimization problems |
| [06-Code-Generation.md](06-Code-Generation.md) | Generate AMPL/Gurobi/Python code |
| [07-Examples.md](07-Examples.md) | Complete example code |
| [08-Best-Practices.md](08-Best-Practices.md) | Performance optimization and coding standards |
| [09-Troubleshooting.md](09-Troubleshooting.md) | Common problems and solutions |

---

## Quick Start

### Installation

```bash
# Install Scala dependencies
conda install -c conda-forge scala=2.13 openjdk

# Install Python version (optional)
pip install ormdsl-py
```

### Your First Model

```python
from ormdsl import *

# Define problem
model = Model("knapsack")

# Add decision variables
x = model.addVars(["item1", "item2", "item3"], vtype="binary")

# Set objective (maximize profit)
model.setObjective(10*x["item1"] + 20*x["item2"] + 15*x["item3"], sense="maximize")

# Add constraint (weight limit)
model.addConstr(2*x["item1"] + 4*x["item2"] + 3*x["item3"] <= 10)

# Generate and solve
code = model.toAMPL()
model.solve(solver="gurobi")
```

### Run Examples

```bash
# Knapsack problem
python examples/knapsack.py

# Facility location
python examples/facility_location.py
```

---

## Key Features

| Feature | Description |
|---------|-------------|
| **Constrained Set Class (CS Class)** | Directly model problem domain concepts as classes |
| **Symbolic Auto-Rewriting** | Automatically optimize constraint expressions |
| **Multi-Language Code Generation** | Support AMPL, Gurobi, Python, LaTeX |
| **Declarative Syntax** | Expression style close to mathematical notation |
| **Modular Design** | Constraints are reusable and composable |

---

## Project Structure

```
ORMDSL/
├── ormdsl-ast/          # Abstract Syntax Tree definitions
├── ormdsl-intepreter/   # Interpreter and code generators
├── ormdsl-py/           # Python implementation
├── ormdsl-gurobi/       # Gurobi integration
├── docs/                # User documentation (this directory)
└── examples/            # Example code
```

---

## Documentation Navigation

### New Users
1. Start with [01-Overview.md](01-Overview.md) to understand the project
2. Learn [02-Core-Concepts.md](02-Core-Concepts.md) to master the basics
3. Follow [03-Quick-Start.md](03-Quick-Start.md) to complete your first model

### Experienced Users
- Check [04-API-Reference.md](04-API-Reference.md) for complete API
- Reference [07-Examples.md](07-Examples.md) for specific problem solutions
- Read [08-Best-Practices.md](08-Best-Practices.md) for performance optimization

---

## Related Resources

| Resource | Link |
|----------|------|
| Project Home | [GPUSolverDSL](../README.md) |
| Academic Paper | [dslpaper/dsl.tex](../dslpaper/dsl.tex) |
| Gurobi Integration | [ormdsl-gurobi](../ormdsl-gurobi/README.md) |
| Gurobi Documentation | https://www.gurobi.com/documentation/ |
| AMPL Documentation | https://ampl.com/resources/hooking-your-solver/ |

---

## Contributing Guide

We welcome Issues and Pull Requests!

1. Fork this repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit changes (`git commit -m 'Add amazing feature'`)
4. Push the branch (`git push origin feature/amazing-feature`)
5. Create a Pull Request

---

*Last updated: 2026-04-06*
