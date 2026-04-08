trait Exp {
  def =:=(that: Exp) = Comparison(this, "==", that)

  def *(that: Exp) = BinaryExp(this, "*", that)

  def +(that: Exp) = BinaryExp(this, "+", that)

  def -(that: Exp) = BinaryExp(this, "-", that)

  def /(that: Exp) = BinaryExp(this, "/", that)

  def <(that: Exp) = BinaryExp(this, "<", that)

  def <=(that: Exp) = BinaryExp(this, "<=", that)

  def >=(that: Exp) = BinaryExp(this, ">=", that)

  def >(that: Exp) = BinaryExp(this, ">", that)

  def ~=(that: Exp) = BinaryExp(this, "~=", that)
}

case class BinaryExp(left: Exp, op: String, right: Exp)

case class ConstExp(x: Int) extends Exp

case class SetElem[X](set: CSet[X], index: Int) extends Exp

case class SummedExp[X](set: CSet[X]) extends Exp

object Binary extends Exp

trait Constraint

case class Comparison(left: Exp, op: String, right: Exp) extends Constraint

object CInt extends Exp

trait CSet[X] {
  val typ: X

  def map[Y](f: X => Y) = MappedSet(f(typ), this)

  def zip[Y](that: CSet[Y]) = ZippedSet((typ, that.typ), this, that)

  def sort[Y](f: X => Y) = SortedSet(typ, this, f)

  def filter(f: X => Constraint) = FilteredSet(typ, this, f)

  def apply(x: Int) = SetElem(this, x)

  def sum = SummedExp(this)
}

case class PrimitiveSet[X](typ: X) extends CSet[X]

case class ZippedSet[X, Y](typ: (X, Y), left: CSet[X], right: CSet[Y]) extends CSet[(X, Y)]

case class MappedSet[X, Y](typ: Y, set: CSet[X]) extends CSet[Y]

case class SortedSet[X, Y](typ: X, set: CSet[X], f: X => Y) extends CSet[X]

case class FilteredSet[X](typ: X, set: CSet[X], f: X => Constraint) extends CSet[X]

// =====================================================================================

object Consumer {
  val weight = CInt
  val distance = PrimitiveSet(CInt)
}

object Warehouse {
  val interdicted = Binary
}

object Main {

  def main(args: Array[String]) {
    val warehouses = PrimitiveSet(Warehouse)

    val c = PrimitiveSet(Consumer).map(c => {

      val w = warehouses.zip(c.distance)
        .sort({ case (w, d) => d })
        .filter({ case (w, d) => w.interdicted =:= ConstExp(0) })
        .map({ case (w, d) => w })

      c.weight * w(0)
    })

    c.sum
  }
}