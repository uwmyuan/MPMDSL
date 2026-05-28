# ORMDSL: DSL AST vs IR Comparison Document

**Date**: 2026-05-21
**Project**: ORMDSL (Optimization Modeling DSL)
**Author**: Yun (yun.yuan@dlmu.edu.cn)

---

## 1. Overview

ORMDSL adopts a **two-layer syntax structure**:

| Layer | Name | File | Purpose |
|-------|------|------|---------|
| **Upper** | DSL AST | `ast grammar.txt` (lines 1-50) | User-friendly modeling syntax (syntactic sugar layer) |
| **Lower** | Algebraic IR | `ast grammar.txt` (lines 52-149) + `IntermediateRepresentation.scala` | Internal compiler representation for code generation |

**Design Goals**:
- DSL syntax: concise and intuitive (close to mathematical formulas)
- IR syntax: rigorous and complete (supports validation, transformation, code generation)

---

## 2. Syntax Comparison Table

### 2.1 Arithmetic Operators

| DSL AST | IR | Description |
|----------|-----|-------------|
| `Plus` | `PlusIR` | Addition |
| `Minus` | `MinusIR` | Subtraction |
| `Times` | `TimesIR` | Multiplication |
| `Div` | `DivIR` | Division |

**Code Example**:
```scala
// DSL (syntactic sugar)
val obj = x + y * z

// IR (internal representation)
val obj = AExpIR(x, PlusIR, AExpIR(y, TimesIR, z))
```

---

### 2.2 Comparison Operators

| DSL AST | IR | Description |
|----------|-----|-------------|
| `Less` | `LessIR` | `<` |
| `Greater` | `GreaterIR` | `>` |
| `LessEq` | `LessEqIR` | `<=` |
| `GreaterEq` | `GreaterEqIR` | `>=` |
| `Equal` | `EqualIR` | `===` |
| `NotEqual` | ❌ Not supported | Supported in sugar layer, not implemented in IR |

**Note**: `NotEqualIR` does not exist in IR; needs to be converted to `LessIR` or `GreaterIR` in the frontend.

---

### 2.3 Expressions

| DSL AST | IR | Description |
|----------|-----|-------------|
| `AExp(e1, op, e2)` | `AExpIR(e1, op, e2)` | Arithmetic expression |
| `PowExp(e, n)` | `PowExpIR(e, n)` | Power operation |
| `VecElem(v, indices)` | `VectorElementIR(v, indices)` | Vector element access |
| `Vec(v)` | `VectorIR(v)` | Vector/variable |
| `Const(d)` | `ConstIR(d)` | Constant |
| `Sum(idx, e)` | `SumIR(idx, e)` | Summation |

**Code Example**:
```scala
// DSL
val obj = Sum(i, x(i) * d(i))

// IR
val obj = SumIR(i, AExpIR(VectorElementIR(x, List(i)),
                         TimesIR,
                         VectorElementIR(d, List(i))))
```

---

### 2.4 Variable Declarations

| DSL AST | IR | Description |
|----------|-----|-------------|
| `InputVar(name)` | `DoubleNum(name)` / `IntegerNum(name)` | Input parameter |
| `IntegerVariable(name, lb, ub)` | `IntegerDecisionVariable(name, lb, ub)` | Integer variable |
| `ContinuousVariable(name, lb, ub)` | `DoubleDecisionVariable(name, lb, ub)` | Continuous variable |
| `BinaryVar(name)` | `IntegerDecisionVariable(name, 0, 1)` | Binary variable |

**Key Difference**:
- DSL uses unified interface `IntegerVariable`; IR explicitly distinguishes `IntegerDecisionVariable` and `DoubleDecisionVariable`
- DSL's `BinaryVar` is syntactic sugar; in IR it's actually `IntegerDecisionVariable(lb=0, ub=1)`

---

### 2.5 Indices

| DSL AST | IR | Description |
|----------|-----|-------------|
| `Index(name, set)` | `IndexIR(name, inputSet)` | Index variable |
| `Set(int, name)` | `IntegerSet(name, lb, ub)` | Integer set |
| `List(indices), name` | `CompoundQualifier(name, list)` | Compound index |

**Code Example**:
```scala
// DSL
val i = Index("i", Set(10, "I"))

// IR
val I = IntegerSet("I", 1, 10)
val i = IndexIR("i", I)
```

---

### 2.6 Constraints

| DSL AST | IR | Description |
|----------|-----|-------------|
| `EquStm(e1, op, e2)` | `SimpleConstraint(name, EquationIR(e1, op, e2))` | Simple constraint |
| `EquStms(equStms, equStm)` | `List[Constraint]` | Constraint list |
| N/A | `QualifiedConstraint(name, equation, qualifier)` | Constraint with qualifier |
| N/A | `DecisionVariableConstraint(name, equation, dv)` | Variable-associated constraint |

**IR Enhanced Features**:
- `QualifiedConstraint`: supports `for` loop qualifiers (e.g., `forAll i in I`)
- `DecisionVariableConstraint`: associates constraint with specific decision variable

---

### 2.7 Objective Function

| DSL AST | IR | Description |
|----------|-----|-------------|
| `ObjStm(min/max, e)` | `MinObjectiveIR(e)` / `MaxObjectiveIR(e)` | Objective function |

**Code Example**:
```scala
// DSL
val f = Formula(min(exp), List(c1, c2))

// IR
val f = FormulaIR(declarations, MinObjectiveIR(exp), List(c1, c2))
```

---

### 2.8 Formula

| DSL AST | IR | Description |
|----------|-----|-------------|
| `FormulaStm(obj, equStms)` | `FormulaIR(declarations, objective, constraints)` | Complete optimization model |

**Key Difference**:
- DSL's `FormulaStm` separates objective function and constraints
- IR's `FormulaIR` explicitly contains all declarations (`declarations`), facilitating scope checking

---

## 3. Validation Function Comparison

### 3.1 DSL Layer Validation (Syntactic Sugar Layer)

**File**: `ORMDSL/ormdsl-py/sugar.py`

| Validation Item | Error Code | Description |
|----------------|------------|-------------|
| Empty objective | E008 | `minimize()` or `maximize()` is empty |
| Empty set | E009 | `set()` parameter is empty list |
| No constraint warning | W001 | Model has no constraints |
| Bounds inversion | E009 | `lb > ub` |
| Integer bounds with decimals | E013 | Integer variable bounds are not integers |
| Negative index | E020 | Array index `< 0` |
| Duplicate variable name | E023 | Variable name declared twice |

**Characteristics**:
- Lightweight validation (fast feedback)
- User-friendly error messages
- Detection rate: ~19% (8/42 error patterns)

---

### 3.2 IR Layer Validation (Compiler Layer)

**File**: `ormdsl-intepreter/src/IntermediateRepresentation.scala`

| Validation Item | Method | Description |
|----------------|--------|-------------|
| Duplicate variable names | `checkDuplicateNames()` | Checks for duplicates in all declarations |
| Undefined variables | `checkUndefinedVariables()` | Ensures all variables in expressions are declared |
| Variable bounds validity | `validateBounds()` | Checks `lb <= ub` |
| Binary variable bounds | `validateBinaryBounds()` | Checks `0 <= x <= 1` |
| Integer variable bounds | `validateIntegerBounds()` | Checks bounds are integers |
| Index range | `VectorElementIR()` constructor | Checks index is within set range |
| Objective association | `checkObjective()` | Ensures objective references decision variables |
| Constraint association | `checkConstraints()` | Ensures each constraint references decision variables |

**Characteristics**:
- Deep validation (comprehensive checking)
- Detailed error reports for developers
- Detection rate: ~74% (31/42 error patterns)

---

## 4. Code Generation Path

### 4.1 DSL → IR Transformation

```
DSL Syntax (sugar.py)
    ↓
Parsing (AST)
    ↓
Desugaring (syntactic sugar removal)
    ↓
IR (IntermediateRepresentation.scala)
    ↓
Validation (IRValidator)
```

### 4.2 IR → Target Code Transformation

```
IR (FormulaIR)
    ↓
IR2AMPL.printAMPL()   → AMPL code
    ↓
IR2Tex.printTex()      → LaTeX mathematical formulas
    ↓
IR2CUOPT.scala        → cuOpt GPU code (in development)
```

---

## 5. Usage Recommendations

### 5.1 When to Use DSL Syntax (Syntactic Sugar Layer)

✅ **Recommended Scenarios**:
- Rapid prototyping and modeling
- Teaching and demonstration
- Simple optimization problems

✅ **Advantages**:
- Concise code (reduces 52%~69% code volume)
- Close to mathematical formulas
- Fast error feedback

---

### 5.2 When to Use IR Syntax (Compiler Layer)

✅ **Recommended Scenarios**:
- Need fine-grained validation control
- Code generation (AMPL, LaTeX, cuOpt)
- Large-scale optimization problems
- Need complete error detection

✅ **Advantages**:
- Complete validation (detection rate 74% vs 19%)
- Supports complex constraint qualifiers
- Directly corresponds to internal representation

---

## 6. Example Comparison

### 6.1 Transportation Problem

**DSL Syntax**:
```scala
val i = Index("i", 1 to 3)      // supply nodes
val j = Index("j", 1 to 4)      // demand nodes
val x = ContinuousVariable("x", 0) // shipment quantity
val c = InputVar("c")             // transportation cost
val a = InputVar("a")             // supply capacity
val b = InputVar("b")             // demand requirement

// Objective: minimize total cost
val obj = minimize(Sum(i, Sum(j, x(i,j) * c(i,j))))

// Constraint: supply limit
val c1 = ForAll(i, Sum(j, x(i,j)) <= a(i))

// Constraint: demand satisfaction
val c2 = ForAll(j, Sum(i, x(i,j)) >= b(j))

val model = Formula(obj, List(c1, c2))
```

**IR Syntax**:
```scala
// Declarations
val I = IntegerSet("I", 1, 3)
val J = IntegerSet("J", 1, 4)
val i = IndexIR("i", I)
val j = IndexIR("j", J)
val x = DoubleDecisionVariable("x", 0, null)
val c = DoubleNum("c")
val a = DoubleNum("a")
val b = DoubleNum("b")

// Objective
val obj = MinObjectiveIR(
  SumIR(i, SumIR(j,
    AExpIR(VectorElementIR(x, List(i,j)),
           TimesIR,
           VectorElementIR(c, List(i,j)))))
)

// Constraint 1: supply limit
val q1 = SetQualifier("q1", i, 1, 3)
val c1 = QualifiedConstraint(
  "c1",
  EquationIR(SumIR(j, VectorElementIR(x, List(i,j))), LessEqIR, VectorElementIR(a, List(i))),
  q1
)

// Constraint 2: demand satisfaction
val q2 = SetQualifier("q2", j, 1, 4)
val c2 = QualifiedConstraint(
  "c2",
  EquationIR(SumIR(i, VectorElementIR(x, List(i,j))), GreaterEqIR, VectorElementIR(b, List(j))),
  q2
)

// Complete model
val model = FormulaIR(
  declarations = List(I, J, i, j, x, c, a, b),
  objective = obj,
  constraints = List(c1, c2)
)
```

**Code Volume Comparison**:
| Syntax | Lines | Characters |
|--------|-------|------------|
| DSL | 18 lines | ~420 chars |
| IR | 52 lines | ~1250 chars |
| **Reduction** | **65%** | **66%** |

---

## 7. Known Limitations and Future Work

### 7.1 Current Limitations

| Limitation | Impact | Priority |
|-------------|--------|----------|
| `NotEqual` not implemented in IR | Cannot represent `x != y` | High |
| DSL performance for large-scale problems | DSL layer overhead ~5-10% | Medium |
| IR syntax verbosity | Steep learning curve | Medium |
| Insufficient debugging info | Difficult error localization | High |

### 7.2 Future Work

1. **Automated DSL → IR transformation**: Develop desugar tool
2. **IR visualization**: Graphviz output of IR structure
3. **More code generation backends**: Gurobi, CPLEX, SCIP
4. **GPU acceleration integration**: cuOpt code generation (in progress)

---

## 8. Reference Files

| File | Path | Description |
|------|------|-------------|
| DSL AST definition | `ormdsl-intepreter/src/ast grammar.txt` (lines 1-50) | DSL syntactic sugar layer |
| IR definition | `ormdsl-intepreter/src/ast grammar.txt` (lines 52-149) | Algebraic IR syntax |
| IR implementation | `ormdsl-intepreter/src/IntermediateRepresentation.scala` | Scala implementation |
| DSL implementation | `ORMDSL/ormdsl-py/sugar.py` | Python syntactic sugar layer |
| AMPL generation | `ormdsl-intepreter/src/IR2AMPL.scala` | IR → AMPL |
| LaTeX generation | `ormdsl-intepreter/src/IR2Tex.scala` | IR → LaTeX |
| cuOpt generation | `ORMDSL/ormdsl-cuopt/src/IR2CUOPT.scala` | IR → cuOpt GPU |

---

## 9. Summary

| Dimension | DSL AST | IR |
|-----------|----------|-----|
| **Target User** | Modelers, students | Compiler developers, advanced users |
| **Syntax Conciseness** | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **Validation Completeness** | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Code Generation Capability** | ❌ Not supported | ✅ Multi-backend support |
| **Learning Curve** | Gentle | Steep |
| **Error Detection Rate** | ~19% | ~74% |
| **Performance (large-scale)** | Slower (sugar overhead) | Fast (direct IR) |

**Recommended Workflow**:
1. Use **DSL** for rapid prototyping
2. Use **IR** for validation and code generation
3. Use **IR** for large-scale problem solving

---

**Document Version**: 1.0
**Last Updated**: 2026-05-21
**Contact**: Yun (yun.yuan@dlmu.edu.cn)
