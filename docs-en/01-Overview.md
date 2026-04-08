# ORMDSL Project Overview

## 1. Project Introduction

**ORMDSL (Object-Relational Modeling for Discrete Optimization Domain Specific Language)** is a domain-specific language for mathematical programming modeling.

### 1.1 Design Goals

ORMDSL aims to solve three core problems in mathematical optimization modeling:

| Problem | Description | ORMDSL Solution |
|---------|-------------|------------------|
| **Impedance Mismatch** | Gap between problem domain concepts and solver APIs | CS Classes directly map domain concepts |
| **Formula Gap** | Difficulty in mapping academic paper formulas to code | Declarative syntax close to mathematical symbols |
| **Code Reuse** | Need to rewrite similar constraints for similar problems | Modular constraint sets are reusable |

### 1.2 Use Cases

✅ **Recommended for**
- Rapid verification of optimization models in academic research
- Scenarios requiring multi-format output (AMPL/Python/LaTeX)
- Projects pursuing high modularity and code reuse
- Problem domains with clear concepts (logistics, scheduling, network flow)

⚠️ **Use with caution**
- Large-scale industrial optimization problems (use Pyomo/GAMS instead)
- Hybrid problems requiring complex constraint programming/heuristic algorithms
- Scenarios with extremely high real-time performance requirements

---

## 2. Core Features

### 2.1 Constrained Set Class (CS Class)

The Constrained Set Class (CS Class) is ORMDSL's core innovation. It directly models problem domain concepts as classes:

```python
# Traditional approach (Pyomo style)
model.x = Var(I, J)
model.c = Constraint(expr=sum(model.x[i,j] for i in I for j in J) <= W)

# ORMDSL approach (domain-driven)
class Vessel extends CS:
    arrival_time: DecisionVariable
    departure_time: DecisionVariable
    berth: DecisionVariable

class Berth extends CS:
    capacity: Parameter
    available_time: Parameter
```

### 2.2 Symbolic Auto-Rewriting

ORMDSL supports automatic symbolic-level optimization:

```python
# Automatic Big-M rewriting for implication constraints
# DSL level: if (x == 1) then (y >= a)
# Auto-rewritten to: y >= a * x
```

### 2.3 Multi-Language Code Generation

One model, multiple outputs:

| Target | Use Case | Generator |
|--------|----------|-----------|
| AMPL | Academic research, solver interface | `AST2AMPL.scala` |
| Gurobi | Industrial-grade solving | `IR2Gurobi.scala` |
| Python | Production environment | `ormdsl-py/` |
| LaTeX | Paper formula display | `AST2Tex.scala` |

### 2.4 Declarative Syntax

```python
# Expression close to mathematical symbols
MIN customers.FOLD(+, c → c.demand * travel[c, allocation(c)])

# Constraint declaration
CONSTRAINT size(established_warehouse) == P
```

---

## 3. Technical Architecture

### 3.1 System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      DSL Source Code                         │
│                    (Domain Problem)                          │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                   Parser (Parser)                            │
│                   ormdsl-syn-dev/                            │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│               Abstract Syntax Tree (AST)                     │
│                    ormdsl-ast/                              │
│   ┌──────────────┬──────────────┬──────────────────┐       │
│   │  Expression  │  Constraint  │  Declaration      │       │
│   └──────────────┴──────────────┴──────────────────┘       │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              Intermediate Representation (IR)                 │
│              ormdsl-intepreter/src/                         │
│   ┌──────────────┬──────────────┬──────────────────┐       │
│   │    ExpIR     │  Constraint  │  ObjectiveIR      │       │
│   └──────────────┴──────────────┴──────────────────┘       │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                 Symbolic Reformulator                       │
│              ⚠️ Partial - symbolic rewrite module pending    │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                    Code Generators                          │
│   ┌──────────┬──────────┬──────────┬────────────┐          │
│   │  AMPL    │ Gurobi   │ Python   │   LaTeX    │          │
│   └──────────┴──────────┴──────────┴────────────┘          │
└─────────────────────────────────────────────────────────────┘
```

### 3.2 Core Modules

| Module | Path | Function |
|--------|------|----------|
| `ormdsl-ast` | `ormdsl-ast/src/` | AST data structure definitions |
| `ormdsl-intepreter` | `ormdsl-intepreter/src/` | IR definitions and code generators |
| `ormdsl-new` | `ormdsl-new/src/` | New version AST implementation |
| `ormdsl-oop` | `ormdsl-oop/src/` | Object-oriented AST |
| `ormdsl-py` | `ormdsl-py/` | Python version |
| `ormdsl-gurobi` | `ormdsl-gurobi/` | Gurobi solver integration |

---

## 4. Comparison with Existing Tools

### 4.1 Feature Comparison

| Feature | ORMDSL | Pyomo | GAMS | AMPL |
|---------|--------|-------|------|------|
| **Abstraction Level** | Domain objects | Variables/constraints | Algebraic expressions | Algebraic expressions |
| **Constrained Set Class** | ✅ Native support | ⚠️ Requires plugin | ❌ | ❌ |
| **Symbolic Rewriting** | ⚠️ Partial | ❌ | ⚠️ Limited | ❌ |
| **Multi-language Generation** | ✅ 4 types | ❌ | ❌ | ❌ |
| **LaTeX Output** | ✅ | ❌ | ❌ | ⚠️ |
| **Learning Curve** | Medium | Medium | Steep | Steep |
| **Community Ecosystem** | Small | Active | Mature | Mature |
| **Documentation** | ⚠️ Developing | ✅ Complete | ✅ Complete | ✅ Complete |

### 4.2 ORMDSL's Unique Advantages

1. **Domain-Driven Design** - Model using problem domain terminology, not solver concepts
2. **Academic-Friendly** - Reduce formula-to-code translation cost for papers
3. **Multi-Format Output** - One model for both academic and production use
4. **Modular Constraints** - Constraints are reusable and composable

---

## 5. Development Status

### 5.1 Implemented Features ✅

- [x] AST definitions (multiple versions: scala/python/js)
- [x] Intermediate Representation (IR) definition
- [x] AMPL code generation
- [x] Gurobi code generation
- [x] LaTeX formula generation
- [x] Python API
- [x] Basic constraint types
- [x] Objective function definition

### 5.2 Under Development ⚠️

- [ ] Symbolic auto-rewriting module (`???` placeholders)
- [ ] Constraint validation framework
- [ ] DSL parser
- [ ] Complete test coverage

### 5.3 To Be Implemented 🚧

- [ ] Performance benchmarking
- [ ] Parallel/distributed support
- [ ] IDE plugin
- [ ] Web interface

---

## 6. License and Citation

### 6.1 License

This project uses the MIT License.

### 6.2 Citation

If you use ORMDSL in academic research, please cite:

```bibtex
@article{ormdsl2024,
  title={ORMDSL: An Object-Relational Modeling Language for Discrete Optimization},
  author={},
  year={2024}
}
```

---

## 7. Contact

- **Project Home**: https://github.com/your-repo/ormdsl
- **Issue Tracker**: GitHub Issues
- **Academic Collaboration**: [your-email@example.com]

---

## 8. Acknowledgments

ORMDSL is inspired by the following projects:

- [Pyomo](http://www.pyomo.org/) - Python Optimization Modeling Language
- [JuMP](https://jump.dev/) - Julia Mathematical Optimization
- [ZIMPL](https://zimpl.zib.de/) - Constraint Modeling Language
- [MiniZinc](https://www.minizinc.org/) - Constraint Programming
