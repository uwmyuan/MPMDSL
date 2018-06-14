object FormulaIR extends Problem {
  {
    /**
      * check if all decision variables are well defined
      *
      * @return
      */
    def checkDecisionVariables: Boolean = ???

    /**
      * check if all decision variables are related to the objective
      *
      * @return
      */
    def checkObjective: Boolean = ???

    /**
      * check if all constraints are related at least one decision variables
      *
      * @return
      */
    def checkConstraints: Boolean = ???


    /**
      * get all the decision variables
      * @return
      */
    def getDecisionVariables: Map[String, DecisionVariable] = ???

    /**
      * get all the input variables
      * @return
      */
    def getInputVariables: Map[String, InputIR] = ???
  }

  override def getDecisionVariables: Map[String, DecisionVariable] = ???

  override def getInputVariables: Map[String, InputIR] = ???
}

object ExpIR extends Problem{
  override def getDecisionVariables: Map[String, DecisionVariable] = ???
  override def getInputVariables: Map[String, DecisionVariable] = ???
}
