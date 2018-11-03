const Naming = {
    count: 0,
    getName: function () { this.count++; return 'x' + this.count; }
};



class Exp {
    lt(that) { return new Comparison(this, '<=', that); }
    gt(that) { return new Comparison(this, '>=', that); }
    eq(that) { return new Comparison(this, '==', that); }

    plus(that) { return new Plus(this, that); }
    times(that) { return new Times(this, that); }
}

// binary decision variable
class Index extends Exp {
    constructor() {
        super();
        this.name = Naming.getName();
    }
    toString() { return this.name; }
}

/*
 * Decision variable
 */
class Var extends Exp {
    // set: constrained set
    // index: either a number or undefined 
    constructor(set, index, indicators) {
        super();
        this.set = set;
        this.index = index;
        if (!index && !indicators) {
            this.indicators = this.set.values.map(_ => new Index());
        }
        else if (!index && indicators) {
            this.indicators = indicators;
        }
    }
    getConstraints() {
        return this.set.getConstraints().concat(this.index ? [] : this.indicators.map((e, i) => e.gt(this.set.indicators[i])));
    }
    // make a new decision variable using f
    map(f) {
        return new Var(this.set.map(f), this.index, this.indicators);
    }
    toString() {
        return '(' + (this.index ? this.set.indicators[this.index].times(this.set.values[this.index]) :
            this.indicators.map((e, i) => e.times(this.set.values[i])).reduce((a, b) => a.plus(b))) + ')';
    }
}

class Plus extends Exp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return this.left.toString() + '+' + this.right.toString();
    }
    getConstraints() {
        const left = this.left instanceof Exp ? this.left.getConstraints() : [];
        const right = this.right instanceof Exp ? this.right.getConstraints() : [];
        return left.concat(right);
    }
}

class Times extends Exp {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    toString() {
        return this.left.toString() + '*' + this.right.toString();
    }
    getConstraints() {
        const left = this.left instanceof Exp ? this.left.getConstraints() : [];
        const right = this.right instanceof Exp ? this.right.getConstraints() : [];
        return left.concat(right);
    }
}

class Implication {
    constructor(left, right) {
        this.left = left;
        this.right = right;
    }
    toString() {
        return this.left.toString() + ' => ' + this.right.toString();
    }
}

class Comparison {
    constructor(left, op, right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }
    toString() {
        return this.left.toString() + this.op + this.right.toString();
    }
}


/*
 * Constrained set
 */
class Set {
    constructor(values) {
        if (!values) throw "set cannot be empty";

        this.values = values;  // list of values
        this.constraintSets = []; // one constraint set for each value
        this.values.map(() => this.constraintSets.push([]));
        this.constraints = [];    // constraint set for all values

        this.indicators = this.values.map(_ => new Index()); // a list of indicator variable names for constraint output 
    }

    getConstraints() {
        const ret = [];
        this.constraintSets.forEach((cs, i) => {
            const eq = new Comparison(this.indicators[i], '==', 1);
            cs.forEach(c =>
                ret.push(new Implication(eq, c))
            );
        });
        return ret.concat(this.constraints);
    }

    toString() {
        return getConstraints().reduce((a, b) => a + '\n' + b);
    }

    // add constraint to each value
    filter(f) {
        for (var i = 0; i < this.values.length; i++) {
            this.constraintSets[i].push(f(this.values[i]));
        }
        return this;
    }
    // add constraint for all value
    assert(f) {
        this.constraints.push(f(this.values));
        return this;
    }
    // return a decision variable
    get(index) {
        return new Var(this, index);
    }
    // make a copy -- internal use
    clone() {
        const set = new Set(this.values);
        for (var i = 0; i < this.constraintSets.length; i++) {
            this.constraintSets[i].forEach(c => set.constraintSets[i].push(c));
        }
        this.constraints.forEach(c => set.constraints.push(c));
        return set;
    }
    // make a new set using f
    map(f) {
        const set = this.clone();
        set.values = set.values.map(v => f(v));
        return set;
    }
}

/*******  example application **************/


const N = 4; // number of warehouses
const M = 5 // number of consumers
const P = 3; // number of established warehouses

const distances = [];

var until = (from, to) => {
    var ret = [];
    for (var x = from; x < to; x++) ret.push(x);
    return ret;
}

class Warehouse {
    constructor(id) {
        this.id = id;
        this.established = new Index();
    }
}

// weight: int
// distances: int[] -- distances from this consumer to each warehouse
// warehouse: decision variable of Warehouse type
class Consumer {
    constructor(weight, distances, warehouse) {
        this.weight = weight;
        this.distances = distances;
        this.warehouse = warehouse;
    }

    factor() {
        return this.warehouse.map(w => this.distances[w.id]).times(this.weight);
    }
}

const warehouses = until(0, N).map(j => new Warehouse(j))

until(0, M).forEach(i => {
    distances[i] = [];
    until(0, N).forEach(j => {
        distances[i][warehouses[j].id] = Math.round(Math.random() * 10);
    })
});

const weights = [];

until(0, M).forEach(i => { weights[i] = Math.round(Math.random() * 10); });

const warehouseSet =
    new Set(warehouses)
        .assert( // make sure total established warehouses is less than P
            ws => ws.map(w => w.established).reduce((a, b) => a.plus(b)).lt(P)
        ) // make sure that only established warehouses are considered
        .filter(w => w.established.eq(1));

const consumers = until(0, M).map(i => new Consumer(weights[i], distances[i], warehouseSet.get()));

const objective = consumers.map(c => c.factor()).reduce((a, b) => a.plus(b));

console.log(objective.toString());

console.log(objective.getConstraints().reduce((a, b) => a + '\n' + b));