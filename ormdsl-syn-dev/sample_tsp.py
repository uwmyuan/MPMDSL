# traveling salesman problem
# input
class Node{
    val visited<<-paths
    }

val warehouse = new Node()
    
val Node=(0 until NUM_EDGE).map(_=>new Node).toSet

# output a set of paths
class path{
    val nodes = new LinkedList[Node]
    def length = nodes.fold(distance(_,_))

val paths: Set[path]=Set()

# constraint
# each node should be visited
assert(paths.fold(_.nodes)==nodes)
# or
nodes.map(n=>assert(n.visited!=None))

# path definition
# path: warehouse -> customer 1-> customer 2-> customer 3 -> ... -> warehouse
# aka warehouse, customer*, warehouse

# starting from warehouse
assert(paths.map(p=>p.nodes[0]==start))

# path back to warehouse
assert(paths.map(p=>p.nodes[p.nodes.size-1]==start))

# all nodes in a path are distinct
paths.map(p=>{
    p.nodes.map(n=>
        p.nodes.map(m=>assert(n!=m))
    )
})

# objective minimize the total length of paths
val objective=paths.fold(_.length+_.length) 