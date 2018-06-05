

trait ModelPrinter{
  def print:String
}
case class AMPLPrinter(f:Formula) extends ModelPrinter{
  def print:String={
    var c=""
//    f.constraints.foreach(c+=_.toAMPL+"\\n")
    return f.objective.toAMPL+c
  }
}
case class TexPrinter(f:Formula) extends ModelPrinter{
  def print:String={
    var c=""
    f.constraints.foreach(c+=_.toTex+"\\n")
    return f.objective.toTex+"\\nsubject to"+c}
}


