package ormgurobi

/**
 * ORMDSL-Gurobi 模块测试
 * 验证代码生成功能
 */

object GurobiTest {
  
  def main(args: Array[String]): Unit = {
    println("=" * 60)
    println("ORMDSL-Gurobi Code Generator Test")
    println("=" * 60)
    
    // 测试1: 基础Gurobi模型
    testBasicModel()
    
    // 测试2: 索引变量
    testIndexedVariables()
    
    // 测试3: 范围约束
    testRangeConstraints()
    
    println("\n✓ All tests completed!")
  }
  
  def testBasicModel(): Unit = {
    println("\n--- Test 1: Basic Knapsack Model ---")
    
    val model = GurobiModel("knapsack")
    
    // 添加决策变量
    model.addVar("x1", lb = 0, ub = 1, Binary, "item 1")
    model.addVar("x2", lb = 0, ub = 1, Binary, "item 2")
    model.addVar("x3", lb = 0, ub = 1, Binary, "item 3")
    
    // 添加约束
    model.addConstraint("c1", "2*x1 + 4*x2 + 3*x3", "<=", "10")
    model.addConstraint("c2", "x1 + x2 + x3", "<=", "2")
    
    // 设置目标函数
    model.setObjective("10*x1 + 20*x2 + 15*x3", Maximize)
    
    // 生成代码
    val code = model.generatePythonCode()
    println(code)
  }
  
  def testIndexedVariables(): Unit = {
    println("\n--- Test 2: Indexed Variables (Facility Location) ---")
    
    val model = GurobiModel("facility_location")
    
    // 添加表达式说明
    model.addExpression("# Number of facilities to open")
    model.addVar("P", lb = 2, ub = 2, Integer)
    
    model.addExpression("# Open facility y[j]")
    model.addIndexedVar("y", "j", lb = 0, ub = 1, Binary)
    
    model.addExpression("# Assign customer i to facility j")
    model.addIndexedVar("x", "i,j", lb = 0, ub = 1, Binary)
    
    // 添加约束
    model.addConstraint("facility_count", "sum(y[j])", "=", "P")
    model.addConstraint("assignment", "sum(x[i,j])", "=", "1")
    model.addConstraint("link", "x[i,j]", "<=", "y[j]")
    
    // 设置目标函数
    model.setObjective("sum(d[i,j] * demand[i] * x[i,j])", Minimize)
    
    val code = model.generatePythonCode()
    println(code)
  }
  
  def testRangeConstraints(): Unit = {
    println("\n--- Test 3: Range Constraints ---")
    
    val model = GurobiModel("range_test")
    
    model.addVar("x", lb = 0, ub = 10, Continuous)
    model.addVar("y", lb = 0, ub = 10, Continuous)
    
    // 范围约束: 5 <= 2*x + 3*y <= 15
    model.addRangeConstraint("c1", "2*x + 3*y", lb = 5, ub = 15)
    
    model.setObjective("x + y", Minimize)
    
    val code = model.generatePythonCode()
    println(code)
  }
  
  def testKnapsackWithData(): Unit = {
    println("\n--- Test 4: Knapsack with Full Implementation ---")
    
    val model = GurobiModel("knapsack_full")
    
    // 物品数据
    val items = List(("item1", 10, 2), ("item2", 20, 4), ("item3", 15, 3), 
                     ("item4", 25, 5), ("item5", 30, 6))
    val capacity = 10
    
    // 生成变量声明
    model.addExpression("# ===== Problem Data =====")
    model.addExpression("items = ['item1', 'item2', 'item3', 'item4', 'item5']")
    model.addExpression("profits = {'item1': 10, 'item2': 20, 'item3': 15, 'item4': 25, 'item5': 30}")
    model.addExpression("weights = {'item1': 2, 'item2': 4, 'item3': 3, 'item4': 5, 'item5': 6}")
    model.addExpression("capacity = 10")
    
    model.addExpression("")
    model.addExpression("# ===== Decision Variables =====")
    model.addExpression("# x[i] = 1 if item i is selected")
    model.addExpression("x = model.addVars(items, vtype=GRB.BINARY)")
    
    model.addExpression("")
    model.addExpression("# ===== Objective Function =====")
    model.addExpression("model.setObjective(gp.quicksum(profits[i] * x[i] for i in items), GRB.MAXIMIZE)")
    
    model.addExpression("")
    model.addExpression("# ===== Constraints =====")
    model.addExpression("model.addConstr(gp.quicksum(weights[i] * x[i] for i in items) <= capacity)")
    
    val code = model.generatePythonCode()
    println(code)
  }
}
