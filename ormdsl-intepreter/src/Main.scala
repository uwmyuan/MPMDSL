import AST2AMPL.printAMPL
import AST2Tex.printTex
import ImplictConst.int2Const

object Main {
  def main(args: Array[String]) {
    println("This is a domain specific language for mathematical programming modeling")

    //AST
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
    println(printTex(f))
//    \[\min{\sum_{i\in I} \sum_{j\in J} X_{ij}*D_{ij}}\]
//    \[\sum_{j\in J} X_{j}=P\]
//    \[Y_{ij}\leq X_{j}\]
//    \[\sum_{j\in J} Y_{ij}=1\]
    println("ampl:")
    println(printAMPL(f))
//    minimize obj: sum {i in I, j in J}X[i,j]*D[i,j];
//    s.t. sum {j in J}X[j]=P;
//    s.t. Y[i,j]<=X[j];
//    s.t. sum {j in J}Y[i,j]=1;

    //IR
//    println("IR")
//    val N = IntegerNum("N", 1, null)
//    val M = IntegerNum("M", 1, null)
//    val P = IntegerNum("P", 1, N)
//    val I = InputSet("I", i, 1, M)
//    val J = InputSet("I", j, 1, N)
//    val D = InputVec("d", d(i, j))
//    print(printAMPL(N.asInstanceOf[Declaration]))
//    print(printAMPL(M.asInstanceOf[Declaration]))
//    print(printAMPL(J))
//    print(printAMPL(I))
//    print(printAMPL(D.asInstanceOf[Declaration]))

    //    param N >= 1, integer;
    //    set Locations := 1 .. N;
    //    param P >= 1 <= N;
    //    param M >= 1, integer;
    //    set Customers := 1 .. M;
    //    param d {Locations, Customers} := Uniform(1.0,2.0);
    //    var x {Locations, Customers} >= 0 <= 1;
    //    var y {Locations} binary;
    //    minimize obj: sum {n in Locations, m in Customers} d[n,m] * x[n,m];
    //    s.t. single_x {m in Customers}: sum {n in Locations} x[n,m] = 1;
    //    s.t. bound_y {n in Locations, m in Customers}: x[n,m] <= y[n];
    //    s.t. num_facilities: sum {n in Locations} y[n] = P;



  }
}
