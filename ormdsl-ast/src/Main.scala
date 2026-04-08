

object Main {
  def main(args: Array[String]) {

    // r-interdiction problem
    val s = BinaryVar("s")
    val d = InputVar("d")
    val a = InputVar("a")
    val x = BinaryVar("x")
    val i = Index("i", Dim("I"))
    val j = Index("j", Dim("F"))
    val k :Index = Index("k", IndexDim(List(i,j), "T"))
    val r = InputVar("r")
    // the paper uses a template of index-set which is not convenient with this AST
    // the size of T varies with indices i,j
    val obj = Sum(i, Sum(j, a(i)*x(i, j) * d(i, j)))
    val c1 = Sum(j, x(i, j)) === Const(1)
    val c2 = Sum(j, s(j)) === r
    val c3 = Sum(k, x(i, k)) <= s(j)
    // this constraint is impacted
    val f = Formula(obj.max, List(c1, c2, c3))

  }
}
