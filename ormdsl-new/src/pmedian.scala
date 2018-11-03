package dsl

trait Allocation  

case class Customer(weight : Int, distances: Vec) {
   val warehouse_no = T.idx(Range(0, distances.values.size))
   
   def weightedDistance = distances.get(warehouse_no) * weight
}

case class Warehouse(warehouse_no: Int) {
  val established = T.idx(Range(0,2))
}

object Main {
  
  def main(args: Array[String]) {
    val n = 7
    val m = 5
    val k = 4
    val r = scala.util.Random
    
    val warehouses = (0 until n).map(x => Warehouse(x))
    val customers = (0 until m).map(x => {
      val distances = T.vec(warehouses.map(_ => Const(r.nextInt(100))).toList)
      Customer(r.nextInt(50), distances)
    })
    
    val objective = Sum(customers.map(c => c.weightedDistance):_*)
    val establishedWarehouses = T.vec(warehouses.map(w => w.established).toList)
    
    val customerWarehouseEstablished = customers.map(c => Const(1) <= establishedWarehouses.get(c.warehouse_no))
    
    val numOfWarehouses = Sum(warehouses.map(x => x.established):_*) <= k
    
    println(T.indices.map(i => i.printConstraint).mkString("\n") + "\n")
    println(T.vectors.map(v => v.printConstraint).mkString("\n") + "\n")

    println(numOfWarehouses + "\n")
    println(customerWarehouseEstablished.mkString("\n") + "\n")        
    println(objective)
  }
  
}