# -*-coding:utf-8-*-
from functools import reduce
import random
count = 0
def getName():
    global count
    count += 1
    return 'x' + str(count)


class Exp:
    def __init__(self):
        pass

    def lt(self, that):
        return Comparison(self,'<=', that)

    def gt(self, that):
        return Comparison(self, '>=', that)

    def eq(self, that):
        return Comparison(self, '==', that)

    def plus(self, that):
        return Plus(self, that)

    def times(self, that):
        return Times(self, that)


# binary decision variable
class Index(Exp):
    def __init__(self):
        self.name = getName()
    
    def toString(self):
         return self.name
   

# Decision variable
class Var(Exp):
    # set: constrained set
    # index: either a number or undefined 
    def __init__(self, Set, index, indicators):
        self.set = Set
        self.index = index
        if not index and not indicators:
            self.indicators = map(lambda x:Index(), self.set)
        elif not index and indicators:
            self.indicators = indicators
        
    
    def getConstraints(self):
        return self.set.getConstraints() + [] if self.index else map(lambda e, i: e.gt(self.set.indicators[i]), self.indicators)
    

    # make a decision variable using f
    def map(self, f):
        return Var(self.set.map(f), self.index, self.indicators)
    

    def toString(self):
        return '(' + ( self.set.indicators[self.index].times(self.set.values[self.index]) if self.index else reduce(lambda a, b: a.plus(b),map(lambda e, i:e.times(self.set.values[i]),self.indicators))) + ')'


class Plus(Exp):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    

    def toString(self):
        return self.left.toString() + '+' + self.right.toString()
    

    def getConstraints(self):
        left = self.left.getConstraints() if isinstance(self.left, Exp) else []
        right = self.right.getConstraints() if isinstance(self.right, Exp) else []
        return left + right
    

class Times(Exp):
    def __init__(self, left, right):
        super()
        self.left = left
        self.right = right

    def toString(self):
        return self.left.toString() + '*' + self.right.toString()

    def getConstraints(self):
        left =  self.left.getConstraints() if  isinstance(self.left, Exp) else []
        right = self.right.getConstraints() if isinstance(self.right, Exp) else []
        return left + right


class Implication:
    def __init__(self,left, right):
        self.left = left
        self.right = right
    
    def toString(self):
        return self.left.toString() + ' => ' + self.right.toString()
    

class Comparison:
    def __init__(self,left, op, right):
        self.left = left
        self.op = op
        self.right = right


    def toString(self):
        return self.left.toString() + self.op + self.right.toString()


# Constrained set
class Set:
    def __init__(self, values):
        if not values:
            raise Exception("set cannot be empty")
        self.values = values  # list of values
        self.constraintSets = [] # one constraint set for each value
        map(lambda x: self.constraintSets.append([]), self.values)
        self.constraints = []    # constraint set for all values
        self.indicators = list(map(lambda x: Index(), self.values)) # a list of indicator variable names for constraint output 

    def getConstraints(self):
        ret = []
        map(lambda cs, i: map(lambda c: ret.append(Implication(Comparison(self.indicators[i], '==', 1), c)), cs), self.constraintSets)
        return ret + self.constraints


    def toString(self):
        return reduce(lambda a,b: a + '\n' + b, self.getConstraints())


    # add constraint to each value
    def filter(self, f):
        for i in range(len(self.values)):
            self.constraintSets.append([])
            self.constraintSets[i].append(f(self.values[i]))
        return self


    # add constraint for all value
    def Assert(self,f):
        self.constraints.append(f(self.values))
        return self


    # return a decision variable    
    def get(self, index):
        return Var(self, index, None)


    # make a copy -- internal use
    def clone(self):
        S = Set(self.values)
        map(lambda c: S.constraintSets.append(c), self.constraintSets)
        map(lambda c: S.constraints.append(c), self.constraints)
        return S


    # make a set using f
    def map(self, f):
        Set = self.clone()
        Set.values = map(lambda v:f(v), Set)
        return Set


#*******  example application **************


N = 4 # number of warehouses
M = 5 # number of consumers
P = 3 # number of established warehouses



class Warehouse:
    def __init__(self, d):
        self.id = id
        self.established = Index()


# weight: int
# distances: int[] -- distances from self consumer to each warehouse
# warehouse: decision variable of Warehouse type
class Consumer:
    def __init__(self, id, weight, distances, warehouse):
        self.id = id
        self.weight = weight
        self.distances = distances
        self.warehouse = warehouse

    def factor(self):
        return self.warehouse.map(lambda w: self.distances[w.id]).times(self.weight)

warehouses = [Warehouse(j) for j in range(N)]

distances=[]
for i in range(M):
    distances.append([])
    for j in range(N):
        distances[i].append(round(random.random() * 10))

weights = []
for i in range(M):
    weights.append(round(random.random() * 10))

# make sure total established warehouses is less than P
# make sure that only established warehouses are considered
warehouseSet = Set(warehouses).Assert(lambda ws: reduce(lambda a,b: a.plus(b), map(lambda w: w.established, ws)).lt("P")).filter(lambda w: w.established.eq(1))

consumers = [Consumer(i, weights[i], distances[i], warehouseSet.get()) for i in range(N)]

objective = reduce(lambda a, b: a.plus(b), consumers.map(lambda c: c.factor()))

print(objective.toString())

print(reduce(lambda a, b: a + '\n' + b, objective.getConstraints()))