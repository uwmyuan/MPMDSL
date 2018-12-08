#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# built-ins


class DecisionVariable:
    def __init__(self, upperbound, lowerbound, discrete):
        self.upperbound = upperbound
        self.lowerbound = lowerbound
        self.discreate = discreate


class Contraint:
    def __init__(self):
        self.expression = None
        self.qualifier = None


class Problem:
    def __init__(self):
        self.objective = None
        self.contraints = []


class Objective:
    def __init__(self, expression, min_or_max):
        self.expression = expression
        self.min_or_max = min_or_max


# each vessel must be assigned a berth segment.
class Allocation(Constraint):
    def __init__(self, resource, consumer):
        self.resource = resource
        self.consumer = consumer


# event in time window constraint template
class InTimeWindow(Constraint):
    def __init__(self, time_window, actual_time):
        self.time_window = time_window
        self.actual_time = actual_time


class AbsoluteDifference(Constraint):
    def __init__(self, terms, value):
        self.terms = terms
        self.value = value


# domain class definition
class Vessel:
    def __init__(self, length, berth_no, berth_position, berth_time_windows,
                 depart_time_windows, water_time_windows, priority, workload,
                 vessel_type):
        self.length = length
        self.berth_no = berth_no
        self.berth_position = berth_position
        self.berth_time_windows = berth_time_windows
        self.depart_time_windows = depart_time_windows
        self.water_time_windows = water_time_windows
        self.priority = priority
        self.workload = workload
        self.vessel_type = vessel_type
        self.arrival_time = DecisionVariable(0, 9999, True)
        self.departure_time = DecisionVariable(0, 9999, True)
        self.time_window = DecisionVariable(0, 9999, True)
        self.time_window_optional = DevisionVariable(0, 9999, True)


class Tide:
    def init(self, date, start_time, end_time):
        self.date
        self.start_time
        self.end_time


class Berth:
    def __init__(self, berth_no, time_available, start_position, end_position,
                 depth, available_for_large_vessel, discreate):
        self.berth_no = berth_no
        self.time_available = time_available
        self.start_position = start_position
        self.end_position = end_position
        self.depth = depth
        self.available_for_large_vessel = available_for_large_vessel
        self.discreate = discreate
        self.QC_capacity


class TimeWindow:
    def __init__(self, index, start_time, end_time, max_num_vessel):
        self.index = index
        self.start_time = start_time
        self.end_time = end_time
        self.max_num_vessel = max_num_vessel


# input
set_of_berth = None
set_of_vessel = None
set_of_time_window = None

# minimize the total weighted departure lateness
obj = Objective(sum([v.priority * v.start_time for v in set_of_vessel]))

# constraint

# each vessel must be assigned a berth segment
c_allocation = [
    Allocation(vessel, berth) for vessel in set_of_vessel
    for berth in set_of_berth
]

# each berth cannot be occupied before it becomes available
c_berth_available = [
    Constraint(vessel.start_time < berth.time_available)
    for vessel in set_of_vessel for berth in set_of_berth
]

# all vessels can berth and depart with satisfactory water levels
c_water_level_arrival = [
    InTimeWindow(w, vessel.arrival_time) for w in vessel.water_times
    if vessel.vessel_type == 'large' for vessel in set_of_vessel
]
c_water_level_departure = [
    InTimeWindow(w, vessel.departure_time) for w in vessel.water_times
    if vessel.vessel_type == 'large' for vessel in set_of_vessel
]

# berthing and departure times of all vessels fall within the feasible time windows provided by the pilot station
c_berth_time = [
    InTimeWindow(w, vessel.arrival_time) for w in vessel.berth_time_windows
    for vessel in set_of_vessel
]
c_departure_time = [
    InTimeWindow(w, vessel.departure_time) for w in vessel.depart_time_windows
    for vessel in set_of_vessel
]

# each time window w, respectively, to be used by a maximum number of vessels
# NOT IN PYTHON SYNTAX
c_num_in_time_window=[Constraint(sum([1 if w == vessel.time_window for vessel in set_of_vessel])<= w.max_num_vessel) for w in set_of_time_window]

# avoiding overlap, either li<= hj or lj <= hi when vessels i and j are assigned to the same berth segment.
# NOT IN PYTHON SYNTAX
c_no_overlap = [Constraint(v1.end_time <= v2.start_time or v2.end_time <= v1.start_time) if v1.berth_no == v2.berth_no for v1 in set_of_vessel for v2 in set_of_vessel]

# a vessel requires single-tide-cycle handling or double-tide-cycle handling
# if double-tide-cycle then the used time windows should consequent two time_windows.
c_double_cycle = [
    AbsoluteDifference([vessel.time_window, vessel.time_window_optional], 1)
    for vessel in set_of_vessel
]

# the allocated QC capacity is sufficient for covering the workload of each vessel
c_within_QC_capacity = [
    Constraint(vessel.capacity >= vessel.departure_time - vessel.arrival_time)
    for vessel in set_of_vessel
]

# collect the problem
p = Problem()
p.objective = obj
p.constraints.extend(c_allocation)
p.constraints.extend(c_berth_available)
p.constraints.extend(c_water_level_arrival)
p.constraints.extend(c_water_level_departure)
p.constraints.extend(c_berth_time)
p.constraints.extend(c_departure_time)
p.constraints.extend(c_num_in_time_window)
p.constraints.extend(c_no_overlap)
p.constraints.extend(c_double_cycle)
p.constraints.extend(c_within_QC_capacity)