package ormcuopt

/**
 * 中间表示(IR)到CUDA代码转换器
 * 将ORMDSL的IR转换为CUDA C++代码
 */

import dsl.ormdsl_intepreter._

object IR2CUOPT {
  
  // 变量类型映射
  private def mapVarType(ir: IntermediateRepresentation): String = ir match {
    case _: IntegerDecisionVariable => "int"
    case _: DoubleDecisionVariable => "float"  // or double for high precision
    case _ => "float"
  }
  
  // 生成CUDA变量声明
  def generateVarDecl(v: VariableIR, prefix: String = "d_"): String = {
    val vtype = mapVarType(v)
    val name = sanitizeName(v.name)
    v match {
      case vec: VectorIR =>
        val size = vec.size.map(s => s"[${s}]").getOrElse("")
        s"$vtype* ${prefix}${name}$size"
      case _ =>
        s"$vtype* ${prefix}${name}"
    }
  }
  
  // 表达式转CUDA代码
  def generate(exp: ExpIR): String = exp match {
    case ConstIR(n) => 
      if (n % 1 == 0) n.toInt.toString else s"${n}f"
    
    case v: IntegerDecisionVariable => 
      s"d_${sanitizeName(v.name)}[i]"
    
    case v: DoubleDecisionVariable => 
      s"d_${sanitizeName(v.name)}[i]"
    
    case v: VectorElementIR =>
      val indices = v.indices.map { idx => s"[${idx.name}_idx]"].mkString
      s"d_${sanitizeName(v.v.toString)}$indices"
    
    case SumIR(idx, e) =>
      s"reduce_sum(${generate(e)})"
    
    case AExpIR(e1, op, e2) =>
      val left = generate(e1)
      val right = generate(e2)
      val opStr = op match {
        case PlusIR => "+"
        case MinusIR => "-"
        case TimesIR => "*"
        case DivIR => "/"
      }
      s"($left $opStr $right)"
    
    case PowExpIR(e, n) =>
      s"powf(${generate(e)}, ${n}f)"
    
    case _ => "0.0f"
  }
  
  // 约束转CUDA kernel代码
  def generateConstraintKernel(constraint: Constraint, kernelName: String): String = {
    constraint match {
      case SimpleConstraint(name, equation) =>
        val (lhs, op, rhs) = generateEquation(equation)
        generateKernel(kernelName, lhs, op, rhs, name)
      
      case QualifiedConstraint(name, equation, qualifier) =>
        val (lhs, op, rhs) = generateEquation(equation)
        val cond = generateQualifier(qualifier)
        generateKernelWithCondition(kernelName, lhs, op, rhs, cond, name)
      
      case _ => ""
    }
  }
  
  // 生成方程三元组
  private def generateEquation(eq: EquationIR): (String, String, String) = {
    val lhs = generate(eq.left)
    val rhs = generate(eq.right)
    val op = eq match {
      case EquationIR(_, op: CopIR, _) => op match {
        case LessIR => "<="
        case GreaterIR => ">="
        case LessEqIR => "<="
        case GreaterEqIR => ">="
        case EqualIR => "=="
      }
    }
    (lhs, op, rhs)
  }
  
  // 生成限定符条件
  private def generateQualifier(q: Qualifier): String = q match {
    case SetQualifier(name, idx, lower, upper) =>
      val lb = generate(lower)
      val ub = generate(upper)
      s"(${lb} <= ${idx.name} && ${idx.name} <= ${ub})"
    case CompoundQualifier(name, qualifiers) =>
      qualifiers.map(generateQualifier).mkString(" && ")
  }
  
  // 生成CUDA kernel
  private def generateKernel(name: String, lhs: String, op: String, rhs: String, 
                             constraintName: String): String = {
    s"""
__global__ void ${sanitizeName(name)}(
    float* d_violation, int* d_violation_count,
    float* d_lhs, float* d_rhs,
    int n
) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= n) return;
    
    // Evaluate constraint: $lhs $op $rhs
    bool violated = !(${lhs} $op ${rhs});
    
    if (violated) {
        int count = atomicAdd(d_violation_count, 1);
        d_violation[count] = idx;
    }
}
"""
  }
  
  // 生成带条件的CUDA kernel
  private def generateKernelWithCondition(name: String, lhs: String, op: String, 
                                          rhs: String, cond: String,
                                          constraintName: String): String = {
    s"""
__global__ void ${sanitizeName(name)}(
    float* d_violation, int* d_violation_count,
    int* d_indices,
    float* d_params,
    int n
) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= n) return;
    
    // Check qualifier condition
    if ($cond) {
        // Evaluate constraint
        bool violated = !(${lhs} $op ${rhs});
        
        if (violated) {
            int count = atomicAdd(d_violation_count, 1);
            d_violation[count] = i;
        }
    }
}
"""
  }
  
  // 目标函数转CUDA代码
  def generateObjectiveKernel(obj: ObjectiveIR, kernelName: String): String = {
    val expr = generate(obj match {
      case MinObjectiveIR(e) => e
      case MaxObjectiveIR(e) => e
    })
    
    obj match {
      case MinObjectiveIR(_) =>
        s"""
__global__ void ${sanitizeName(kernelName)}_objective(
    float* d_x, float* d_result, int n
) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx != 0) return;
    
    float obj = $expr;
    d_result[0] = obj;
}
"""
      
      case MaxObjectiveIR(_) =>
        s"""
__global__ void ${sanitizeName(kernelName)}_objective(
    float* d_x, float* d_result, int n
) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx != 0) return;
    
    float obj = $expr;
    d_result[0] = obj;
}
"""
    }
  }
  
  // 生成归约操作
  def generateReduceKernel(op: String = "sum"): String = {
    val identity = op match {
      case "sum" => "0.0f"
      case "min" => "INF"
      case "max" => "NEG_INF"
    }
    val reduceOp = op match {
      case "sum" => "+"
      case "min" => "min(a, b)"
      case "max" => "max(a, b)"
    }
    
    s"""
// Parallel reduction kernel for $op
__global__ void reduce_${op}(float* d_input, float* d_output, int n) {
    extern __shared__ float sdata[];
    
    unsigned int tid = threadIdx.x;
    unsigned int i = blockIdx.x * blockDim.x + threadIdx.x;
    
    sdata[tid] = (i < n) ? d_input[i] : $identity;
    __syncthreads();
    
    for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s && i + s < n) {
            float a = sdata[tid];
            float b = sdata[tid + s];
            sdata[tid] = $reduceOp;
        }
        __syncthreads();
    }
    
    if (tid == 0) d_output[blockIdx.x] = sdata[0];
}
"""
  }
  
  // 符号名称清理
  private def sanitizeName(name: String): String = {
    name.replaceAll("[^a-zA-Z0-9_]", "_")
  }
}
