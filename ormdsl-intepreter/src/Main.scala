import ImplictConst.int2Const

object Main {
  def main(args: Array[String]) {
    println("This is a domain specific language for mathematical programming modeling")

    val x = BinaryVar("X")
    val d = InputVar("D")
    val y = BinaryVar("Y")
    val i = Index("i", Dim(10, "I"))
    val j = Index("j", Dim(20, "J"))
    //before Syntactic sugar
    //val sum = Sum(i, Sum(j, AExp(VecElem(x, List(i,j)), Times, VecElem(d, List(i,j)))))
    val obj = Sum(i, Sum(j, x(i, j) * d(i, j)))

    val p = InputVar("P")
    val c1 = Sum(j, x(j)) === p
    val c2 = y(i, j) <= x(j)
    val c3 = Sum(j, y(i, j)) === 1

    val f = Formula(obj.min, List(c1, c2, c3))

    println("latex:")
    println(f.toTex)
    println("IR latex:")
    println(f.toFormulaIR.toTex)
    println("ampl:")
    println(f.toAMPL)
    println(f.toFormulaIR.toAMPL)


  }
}
