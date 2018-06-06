

trait ModelPrinter{
  def print:String
}
case class AMPLPrinter(f:Formula) extends ModelPrinter{
  def print:String={
    //TODO declare var
    var c=""
    f.constraints.foreach(c+=_.toAMPL+"\n")
    //merge the sums
    return (f.objective.toAMPL+"\n"+c).replace("}sum {",", ")
  }
}
case class TexPrinter(f:Formula) extends ModelPrinter{
  def print:String={
    var c=""
    f.constraints.foreach(c+=_.toTex+"\n")
    return f.objective.toTex+"\nsubject to\n"+c}
}


