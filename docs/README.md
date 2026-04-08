# ORMDSL 用户文档

> **ORMDSL (Object-Relational Modeling for Discrete Optimization Domain Specific Language)**
> 
> 一个用于数学规划建模的领域特定语言，通过约束集类实现问题域概念与优化模型的自然映射。

---

## 📚 文档目录

| 文档 | 说明 |
|------|------|
| [01-概述.md](01-概述.md) | 项目介绍、设计理念、核心特性 |
| [02-核心概念.md](02-核心概念.md) | 约束集类、代数表达式、目标函数 |
| [03-快速开始.md](03-快速开始.md) | 5分钟快速入门指南 |
| [04-API参考.md](04-API参考.md) | 完整的API文档 |
| [05-问题建模.md](05-问题建模.md) | 如何建模各类优化问题 |
| [06-代码生成.md](06-代码生成.md) | 生成AMPL/Gurobi/Python代码 |
| [07-示例手册.md](07-示例手册.md) | 完整示例代码 |
| [08-最佳实践.md](08-最佳实践.md) | 性能优化和编码规范 |
| [09-故障排除.md](09-故障排除.md) | 常见问题和解决方案 |

---

## 🚀 快速开始

### 安装

```bash
# 安装Scala依赖
conda install -c conda-forge scala=2.13 openjdk

# 安装Python版本 (可选)
pip install ormdsl-py
```

### 第一个模型

```python
from ormdsl import *

# 定义问题
model = Model("knapsack")

# 添加决策变量
x = model.addVars(["item1", "item2", "item3"], vtype="binary")

# 设置目标函数 (最大化利润)
model.setObjective(10*x["item1"] + 20*x["item2"] + 15*x["item3"], sense="maximize")

# 添加约束 (重量限制)
model.addConstr(2*x["item1"] + 4*x["item2"] + 3*x["item3"] <= 10)

# 生成并求解
code = model.toAMPL()
model.solve(solver="gurobi")
```

### 运行示例

```bash
# 背包问题
python examples/knapsack.py

# 设施选址
python examples/facility_location.py
```

---

## 🔑 核心特性

| 特性 | 描述 |
|------|------|
| **约束集类 (CS Class)** | 将问题域概念直接建模为类 |
| **符号自动重写** | 自动优化约束表达式 |
| **多语言代码生成** | 支持AMPL、Gurobi、Python、LaTeX |
| **声明式语法** | 接近数学公式的表达方式 |
| **模块化设计** | 约束可复用、可组合 |

---

## 📁 项目结构

```
ORMDSL/
├── ormdsl-ast/          # 抽象语法树定义
├── ormdsl-intepreter/   # 解释器和代码生成器
├── ormdsl-py/           # Python实现
├── ormdsl-gurobi/       # Gurobi集成
├── docs/                # 用户文档 (本目录)
└── examples/            # 示例代码
```

---

## 📖 文档导航

### 新用户
1. 从 [01-概述.md](01-概述.md) 开始了解项目
2. 学习 [02-核心概念.md](02-核心概念.md) 掌握基础
3. 跟随 [03-快速开始.md](03-快速开始.md) 完成第一个模型

### 有经验用户
- 查看 [04-API参考.md](04-API参考.md) 获取完整API
- 参考 [07-示例手册.md](07-示例手册.md) 解决特定问题
- 阅读 [08-最佳实践.md](08-最佳实践.md) 优化性能

---

## 🔗 相关资源

| 资源 | 链接 |
|------|------|
| 项目主页 | [GPUSolverDSL](../README.md) |
| 学术论文 | [dslpaper/dsl.tex](../dslpaper/dsl.tex) |
| Gurobi集成 | [ormdsl-gurobi](../ormdsl-gurobi/README.md) |
| Gurobi文档 | https://www.gurobi.com/documentation/ |
| AMPL文档 | https://ampl.com/resources/hooking-your-solver/ |

---

## 📝 贡献指南

欢迎提交 Issue 和 Pull Request！

1. Fork 本仓库
2. 创建功能分支 (`git checkout -b feature/amazing-feature`)
3. 提交更改 (`git commit -m 'Add amazing feature'`)
4. 推送分支 (`git push origin feature/amazing-feature`)
5. 创建 Pull Request

---

*最后更新: 2026-04-06*
