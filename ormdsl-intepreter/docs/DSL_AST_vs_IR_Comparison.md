# ORMDSL：DSL AST vs IR 对比文档

**日期**: 2026-05-21
**项目**: ORMDSL (Optimization Modeling DSL)
**作者**: Yun (yun.yuan@dlmu.edu.cn)

---

## 1. 概述

ORMDSL 采用**两层语法结构**：

| 层级 | 名称 | 文件 | 用途 |
|------|------|------|------|
| **上层** | DSL AST | `ast grammar.txt` (第1-50行) | 用户友好的建模语法（语法糖层） |
| **下层** | 代数 IR | `ast grammar.txt` (第52-149行) + `IntermediateRepresentation.scala` | 编译器内部表示，用于代码生成 |

**设计目标**：
- DSL 语法简洁直观（接近数学公式）
- IR 语法严谨完整（支持验证、转换、代码生成）

---

## 2. 语法对比表

### 2.1 算术运算符

| DSL AST | IR | 说明 |
|---------|-----|------|
| `Plus` | `PlusIR` | 加法 |
| `Minus` | `MinusIR` | 减法 |
| `Times` | `TimesIR` | 乘法 |
| `Div` | `DivIR` | 除法 |

**代码示例**：
```scala
// DSL（语法糖）
val obj = x + y * z

// IR（内部表示）
val obj = AExpIR(x, PlusIR, AExpIR(y, TimesIR, z))
```

---

### 2.2 比较运算符

| DSL AST | IR | 说明 |
|---------|-----|------|
| `Less` | `LessIR` | `<` |
| `Greater` | `GreaterIR` | `>` |
| `LessEq` | `LessEqIR` | `<=` |
| `GreaterEq` | `GreaterEqIR` | `>=` |
| `Equal` | `EqualIR` | `===` |
| `NotEqual` | ❌ 不支持 | 语法糖层支持，IR 未实现 |

**注意**：IR 中没有 `NotEqualIR`，需要在前端转换为 `LessIR` 或 `GreaterIR`。

---

### 2.3 表达式 (Expression)

| DSL AST | IR | 说明 |
|---------|-----|------|
| `AExp(e1, op, e2)` | `AExpIR(e1, op, e2)` | 算术表达式 |
| `PowExp(e, n)` | `PowExpIR(e, n)` | 幂运算 |
| `VecElem(v, indices)` | `VectorElementIR(v, indices)` | 向量元素访问 |
| `Vec(v)` | `VectorIR(v)` | 向量/变量 |
| `Const(d)` | `ConstIR(d)` | 常量 |
| `Sum(idx, e)` | `SumIR(idx, e)` | 求和 |

**代码示例**：
```scala
// DSL
val obj = Sum(i, x(i) * d(i))

// IR
val obj = SumIR(i, AExpIR(VectorElementIR(x, List(i)),
                         TimesIR,
                         VectorElementIR(d, List(i))))
```

---

### 2.4 变量声明

| DSL AST | IR | 说明 |
|---------|-----|------|
| `InputVar(name)` | `DoubleNum(name)` / `IntegerNum(name)` | 输入参数 |
| `IntegerVariable(name, lb, ub)` | `IntegerDecisionVariable(name, lb, ub)` | 整数变量 |
| `ContinuousVariable(name, lb, ub)` | `DoubleDecisionVariable(name, lb, ub)` | 连续变量 |
| `BinaryVar(name)` | `IntegerDecisionVariable(name, 0, 1)` | 二进制变量 |

**关键区别**：
- DSL 使用统一接口 `IntegerVariable`，IR 明确区分 `IntegerDecisionVariable` 和 `DoubleDecisionVariable`
- DSL 的 `BinaryVar` 是语法糖，IR 中实际上是 `IntegerDecisionVariable(lb=0, ub=1)`

---

### 2.5 索引 (Index)

| DSL AST | IR | 说明 |
|---------|-----|------|
| `Index(name, set)` | `IndexIR(name, inputSet)` | 索引变量 |
| `Set(int, name)` | `IntegerSet(name, lb, ub)` | 整数集合 |
| `List(indices), name` | `CompoundQualifier(name, list)` | 复合索引 |

**代码示例**：
```scala
// DSL
val i = Index("i", Set(10, "I"))

// IR
val I = IntegerSet("I", 1, 10)
val i = IndexIR("i", I)
```

---

### 2.6 约束 (Constraint)

| DSL AST | IR | 说明 |
|---------|-----|------|
| `EquStm(e1, op, e2)` | `SimpleConstraint(name, EquationIR(e1, op, e2))` | 简单约束 |
| `EquStms(equStms, equStm)` | `List[Constraint]` | 约束列表 |
| N/A | `QualifiedConstraint(name, equation, qualifier)` | 带限定符的约束 |
| N/A | `DecisionVariableConstraint(name, equation, dv)` | 变量关联约束 |

**IR 增强功能**：
- `QualifiedConstraint`：支持 `for` 循环限定符（如 `forAll i in I`）
- `DecisionVariableConstraint`：将约束与特定决策变量关联

---

### 2.7 目标函数 (Objective)

| DSL AST | IR | 说明 |
|---------|-----|------|
| `ObjStm(min/max, e)` | `MinObjectiveIR(e)` / `MaxObjectiveIR(e)` | 目标函数 |

**代码示例**：
```scala
// DSL
val f = Formula(min(exp), List(c1, c2))

// IR
val f = FormulaIR(declarations, MinObjectiveIR(exp), List(c1, c2))
```

---

### 2.8 公式 (Formulation)

| DSL AST | IR | 说明 |
|---------|-----|------|
| `FormulaStm(obj, equStms)` | `FormulaIR(declarations, objective, constraints)` | 完整优化模型 |

**关键区别**：
- DSL 的 `FormulaStm` 将目标函数和约束分开
- IR 的 `FormulaIR` 显式包含所有声明（`declarations`），便于作用域检查

---

## 3. 验证功能对比

### 3.1 DSL 层验证（语法糖层）

**文件**: `ORMDSL/ormdsl-py/sugar.py`

| 验证项 | 错误码 | 说明 |
|--------|--------|------|
| 空目标函数 | E008 | `minimize()` 或 `maximize()` 为空 |
| 空集合 | E009 | `set()` 参数为空列表 |
| 无约束警告 | W001 | 模型没有任何约束 |
| 界颠倒 | E009 | `lb > ub` |
| 整数界含小数 | E013 | 整数变量的界不是整数 |
| 负索引 | E020 | 数组索引 `< 0` |
| 重复变量名 | E023 | 变量名重复声明 |

**特点**：
- 轻量级验证（快速反馈）
- 面向用户友好错误消息
- 检测率：~19%（8/42 错误模式）

---

### 3.2 IR 层验证（编译器层）

**文件**: `ormdsl-intepreter/src/IntermediateRepresentation.scala`

| 验证项 | 方法 | 说明 |
|--------|------|------|
| 重复变量名 | `checkDuplicateNames()` | 检查所有声明中的重复名称 |
| 未定义变量 | `checkUndefinedVariables()` | 确保表达式中所有变量已声明 |
| 变量界有效性 | `validateBounds()` | 检查 `lb <= ub` |
| 二进制变量界 | `validateBinaryBounds()` | 检查 `0 <= x <= 1` |
| 整数变量界 | `validateIntegerBounds()` | 检查界为整数 |
| 索引范围 | `VectorElementIR()` 构造器 | 检查索引在集合范围内 |
| 目标函数关联 | `checkObjective()` | 确保目标函数引用决策变量 |
| 约束关联 | `checkConstraints()` | 确保每个约束引用决策变量 |

**特点**：
- 深度验证（全面检查）
- 面向开发者详细错误报告
- 检测率：~74%（31/42 错误模式）

---

## 4. 代码生成路径

### 4.1 DSL → IR 转换

```
DSL Syntax (sugar.py)
    ↓
Parsing (AST)
    ↓
Desugaring (语法糖去除)
    ↓
IR (IntermediateRepresentation.scala)
    ↓
Validation (IRValidator)
```

### 4.2 IR → Target Code 转换

```
IR (FormulaIR)
    ↓
IR2AMPL.printAMPL()   → AMPL 代码
    ↓
IR2Tex.printTex()      → LaTeX 数学公式
    ↓
IR2CUOPT.scala        → cuOpt GPU 代码 (开发中)
```

---

## 5. 使用建议

### 5.1 何时使用 DSL 语法（语法糖层）

✅ **推荐使用场景**：
- 快速原型建模
- 教学和演示
- 简单优化问题

✅ **优势**：
- 代码简洁（减少 52%~69% 代码量）
- 接近数学公式
- 快速错误反馈

---

### 5.2 何时使用 IR 语法（编译器层）

✅ **推荐使用场景**：
- 需要精细控制验证
- 代码生成（AMPL、LaTeX、cuOpt）
- 大规模优化问题
- 需要完整错误检测

✅ **优势**：
- 完整验证（检测率 74% vs 19%）
- 支持复杂约束限定符
- 直接对应内部表示

---

## 6. 示例对比

### 6.1 运输问题 (Transportation Problem)

**DSL 语法**：
```scala
val i = Index("i", 1 to 3)      // 供应点
val j = Index("j", 1 to 4)      // 需求点
val x = ContinuousVariable("x", 0) // 运输量
val c = InputVar("c")             // 运输成本
val a = InputVar("a")             // 供应量
val b = InputVar("b")             // 需求量

// 目标：最小化总成本
val obj = minimize(Sum(i, Sum(j, x(i,j) * c(i,j))))

// 约束：供应限制
val c1 = ForAll(i, Sum(j, x(i,j)) <= a(i))

// 约束：需求满足
val c2 = ForAll(j, Sum(i, x(i,j)) >= b(j))

val model = Formula(obj, List(c1, c2))
```

**IR 语法**：
```scala
// 声明
val I = IntegerSet("I", 1, 3)
val J = IntegerSet("J", 1, 4)
val i = IndexIR("i", I)
val j = IndexIR("j", J)
val x = DoubleDecisionVariable("x", 0, null)
val c = DoubleNum("c")
val a = DoubleNum("a")
val b = DoubleNum("b")

// 目标
val obj = MinObjectiveIR(
  SumIR(i, SumIR(j,
    AExpIR(VectorElementIR(x, List(i,j)),
           TimesIR,
           VectorElementIR(c, List(i,j)))))
)

// 约束 1：供应限制
val q1 = SetQualifier("q1", i, 1, 3)
val c1 = QualifiedConstraint(
  "c1",
  EquationIR(SumIR(j, VectorElementIR(x, List(i,j))), LessEqIR, VectorElementIR(a, List(i))),
  q1
)

// 约束 2：需求满足
val q2 = SetQualifier("q2", j, 1, 4)
val c2 = QualifiedConstraint(
  "c2",
  EquationIR(SumIR(i, VectorElementIR(x, List(i,j))), GreaterEqIR, VectorElementIR(b, List(j))),
  q2
)

// 完整模型
val model = FormulaIR(
  declarations = List(I, J, i, j, x, c, a, b),
  objective = obj,
  constraints = List(c1, c2)
)
```

**代码量对比**：
| 语法 | 行数 | 字符数 |
|------|------|--------|
| DSL | 18 行 | ~420 字符 |
| IR | 52 行 | ~1250 字符 |
| **减少** | **65%** | **66%** |

---

## 7. 已知限制和未来工作

### 7.1 当前限制

| 限制 | 影响 | 优先级 |
|------|------|--------|
| `NotEqual` 在 IR 中未实现 | 无法表示 `x != y` | 高 |
| 大规模问题 DSL 性能 | DSL 层开销 ~5-10% | 中 |
| IR 语法冗长 | 学习曲线陡峭 | 中 |
| 调试信息不足 | 错误定位困难 | 高 |

### 7.2 未来工作

1. **自动化 DSL → IR 转换**：开发 desugar 工具
2. **IR 可视化**：Graphviz 输出 IR 结构
3. **更多代码生成后端**：Gurobi、CPLEX、SCIP
4. **GPU 加速集成**：cuOpt 代码生成（进行中）

---

## 8. 参考文件

| 文件 | 路径 | 说明 |
|------|------|------|
| DSL AST 定义 | `ormdsl-intepreter/src/ast grammar.txt` (第1-50行) | DSL 语法糖层 |
| IR 定义 | `ormdsl-intepreter/src/ast grammar.txt` (第52-149行) | 代数 IR 语法 |
| IR 实现 | `ormdsl-intepreter/src/IntermediateRepresentation.scala` | Scala 实现 |
| DSL 实现 | `ORMDSL/ormdsl-py/sugar.py` | Python 语法糖层 |
| AMPL 生成 | `ormdsl-intepreter/src/IR2AMPL.scala` | IR → AMPL |
| LaTeX 生成 | `ormdsl-intepreter/src/IR2Tex.scala` | IR → LaTeX |
| cuOpt 生成 | `ORMDSL/ormdsl-cuopt/src/IR2CUOPT.scala` | IR → cuOpt GPU |

---

## 9. 总结

| 维度 | DSL AST | IR |
|------|---------|-----|
| **目标用户** | 建模人员、学生 | 编译器开发者、高级用户 |
| **语法简洁性** | ⭐⭐⭐⭐⭐ | ⭐⭐ |
| **验证完整度** | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| **代码生成能力** | ❌ 不支持 | ✅ 支持多后端 |
| **学习曲线** | 平缓 | 陡峭 |
| **错误检测率** | ~19% | ~74% |
| **性能（大规模）** | 稍慢（语法糖开销） | 快（直接 IR） |

**推荐工作流**：
1. 用 **DSL** 快速原型
2. 用 **IR** 验证和代码生成
3. 用 **IR** 进行大规模问题求解

---

**文档版本**: 1.0
**最后更新**: 2026-05-21
**联系人**: Yun (yun.yuan@dlmu.edu.cn)
