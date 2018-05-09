import Equality.Equality

class Equation(left:Expression,right:Expression,equal:Equality){
  def leftExpression(): Expression =left
  def rightExpression(): Expression =right
  def equality(): Equality =equal
}
