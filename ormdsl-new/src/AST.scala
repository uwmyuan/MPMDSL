package dsl

trait AST

trait DecisionVariable extends AST

class Index(r: Range, i: Int) extends DecisionVariable with AExp {
  override def toString = "i"+i
  def printConstraint = r.start + " <= " + this + " <= " + (r.end-1)
}

trait Exp extends AST
trait CExp extends Exp

trait AExp extends Exp { 
  def * (e: AExp) = Prod(this, e)
  def * (x: Int) = Prod(this, Const(x))
  def <= (e: AExp) = LE(this, e)
  def <= (x: Int) = LE(this, Const(x))
}

case class LE(e1: AExp, e2: AExp) extends CExp {
  override def toString = e1.toString + " <= " + e2.toString
}

case class Elem(s: Vec, i: Index) extends AExp {
  override def toString = s.toString + "_" + i.toString
}

case class Const(x: Int) extends AExp {
  override def toString = ""+x
}
case class Prod(e1: AExp, e2: AExp) extends AExp {
  override def toString = e1.toString + " * " + e2.toString
}

case class Sum(e: AExp*) extends AExp {
  override def toString = e.mkString(" + ")
}

class Vec(val values: List[AExp], i: Int) extends AST {
  def get(i: Index) = Elem(this, i)
  override def toString = "v"+i
  def printConstraint = this.toString + " = {" + values.mkString(", ") + "}"
}

object T { 
  var indices = List[Index]()
  var vectors = List[Vec]()
  
  def idx(r: Range) = { 
    val ret = new Index(r, indices.size);
    indices = ret::indices
    ret
  }
  
  def vec(v: List[AExp]) = { 
    val ret = new Vec(v, vectors.size)
    vectors = ret::vectors
    ret
  }
}



