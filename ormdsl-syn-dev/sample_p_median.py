# -*- coding: utf-8 -*-

# base problem: p-median problem by Hakimi (1964, 1965) citation: 2455+1141
# find p warehouses out of n potential sites to serve m customers with minimal total weighted transportation cost
class Customer:
    def __init__(self, weight, distance_to_warehouse):
        self.weight = weight
        self.distance_to_warehouse = distance_to_warehouse
        self.warehousr_no = DecisionVariable(lowerbound = 1, upperbound = NUM_WAREHOUSE, category = non_negative_integer)

class Warehouse:
    def __init__(self, warehouse_no):
        self.warehouse_no= warehouse_no
        self.established = DecisionVariable(lowerbound = 0, upperbound = 1, category= binary)

# input 
set_of_customer = None
set_of_warehouse = None
NUM_WAREHOUSE = None


# objective: minimize the total weighted transportation cost
obj = MinObjective(sum([customer.weight*customer.distance_to_warehouse[customer.warehouse_no] for customer in set_of_customer]))

# constraints:
# this is a many-to-one matching since a warehouse may serve multiple customers
# options: one-to-one, many-to-many
# along with the obj, the customer can only be allocated to the nearest warehouse
c_allocation = [Allocation(customer, warehouse) for customer in set_of_customer for warehouse in set_of_warehouse]

# each customer must be allocated to one warehouse
c_non_unserved_customer = [Constraint(customer.warehouse_no!=None) for customer in set_of_customer]

# the number of warehouses where established = True is limited to NUM_WAREHOUSE
c_num_warehouse = [Constraint(sum([warehouse.established for warehouse in set_of_warehouse])==NUM_WAREHOUSE)]

# pack up the problem
p=Problem()
p.add(obj)
p.add(c_allocation)
p.add(c_non_unserved_customer)
p.add(c_num_warehouse)

# Note: this is not a numerically tightest formulation because :
# In the p-median problem, Church proved that certain variables can be combined under specific circumstances. For example, if site 10 is the sixth closest facility for demands 2 and 5, and the set of closer sites is the same for both demands, then it can be proved that if demand 2 assigns to site 10 as its closest facility, then demand 5 will assign to site 10 as its closest facility. For such cases in the p-median problem, only one assignment variable is needed rather than two individual assignment variables. 

#----------------------------------------------
# extended problem 1: r interdiction by Church, Scaparra, and Middleton (2004) citation: 11
# find r interdictions out of p warehouses to induce the maximal total transportation cost
class WarehouseInterdition(Warehouse):
    def __init__(self,warehouse_no):
        super().__init__(self,warehouse_no)
        self.interdicted = DecisionVariable(lowerbound = 0, upperbound = 1, category= binary)
# input
# warehouse.established is known for warehouse in set_of_warehouse
NUM_INTERDICTION = None

# maximize the total transportation cost
obj_r = MaxObjective(sum([customer.weight*customer.distance_to_warehouse[customer.warehouse_no] for customer in set_of_customer]))

# constraint
# for each established warehouse, the customer can only be allcated to farther warehouse if this warehouse is interdicted
c_interdiction = [Constraint(customer.distance_to_warehouse[customer.warehouse_no] <= customer.distance_to_warehouse[warehouse.warehouse_no] for customer in set_of_customer if warehouse.interdicted = False ) for warehouse in set_of_warehouse]

# the number of warehouses where established = True is limited to NUM_WAREHOUSE
c_num_interdiction = [Constraint(sum([warehouse.interdicted and warehouse.established for warehouse in set_of_warehouse])==NUM_WAREHOUSE))]


# pack up the problem
r=Problem()
r.add(c_non_unserved_customer)
r.add(c_interdiction)
r.add(c_num_interdiction)

#----------------------------------------------
# extended problem 2: q fortification by Church and Scaparra (2007) citation: 215
# find q fortifications along with r interdictions to weighted distance

# input
NUM_FORTIFICATION

# objective: allocate fortification resources so that the weighted distance after the worst-case loss of r facilities is minimized
obj_q 

# constraints
# demand i cannot assign to a facility j in B^h_i (set of facilities that have been interdicted in pattern h and are closer than d^h_i units to i) under interdiction pattern h, unless that site j has been fortified
# Note: For possible interdiction patterns that include site 10, improvement in the weighted distance by fortifying site 10 will accrue to both demands 2 and 5.

# a demand assigns to at most one facility assuming that interdiction pattern h occurs. This assignment is made either to the closest noninterdicted site or to the closest fortified site.

# the obj >= the weighted distance of interdiction pattern h, given fortification. 
# If for a given pattern h, some sites in that pattern have been fortified, then that interdiction pattern has been partially thwarted, and the weighted distance is computed associated with this fortification. 