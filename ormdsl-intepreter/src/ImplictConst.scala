object ImplictConst {
  implicit def int2Const(i: Int) = Const(i)
  implicit def int2ConstIR(i: Int) = ConstIR(i)

  implicit def str2Var(s: String) = InputVar(s)
}
