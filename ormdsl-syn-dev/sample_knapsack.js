class expression{
    val expr

    val value

    ...

}


I coded the packing problems as follows.


//1-d discrete packing
class item{
    val weight
    val value
    val selected<<-Boolean
}
//knapsack capacity
val W

//number of items
val n

val items = (0 until n).map(_=>new item).toSet

//knapsack constraint
//if i.selected then i.weight else 0
sum(items.map(i => i.selected*i.weight)) <= W )

val objective = sum(items.map(i => i.selected*i.value))

//3d discrete packing problem---------------------
class Item{
    val weight
    val value
    val selected<<-Boolean
    val length
    val height
    val x
    val y
}

val n

val items = (0 until n).map(_=>new item).toSet

val W

//knapsack length
val L

//knapsack height
val H

assert(sum(items.map(i => i.selected*i.weight)) <= W )

//all items should be in the rectangle L*H
items.map(i=>{
    assert(0<=i.x<=L-i.length)
    assert(0<=i.y<=H-i.height)
})

//no overlapping, two distinct elements i,j in items
items.filter(i,j=>i.selected & j.selected).map(i,j=>{
    assert(i.x+i.length<=j.x or j.x+j.length<i.x)
    assert(i.y+i.height<=j.y or j.y+j.height<i.y)
})

val objective = sum(items.map(i => i.selected*i.value))

// cutting stock problem-----------------------------
// eliminating selected and add L as a decision variable
// change obj fn to min L