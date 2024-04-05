"use strict"
const VARIABLE_NAME = ["x", "y", "z"];

function baseOp(operands, opName) {
    this.operands = operands;
    this.opName = opName;
}
baseOp.prototype.evaluate = function(...args) { 
    let evExpr = [];
    for (const operand of this.operands) {
        evExpr.push(operand.evaluate(...args));
    }
    return evExpr;
}
baseOp.prototype.toString = function() {
    let str = [];
    for (const operand of this.operands) {
        str.push(operand.toString());
    }
    str.push(this.opName);
    return str.join(" ");
}

function Variable(...operands) {
    baseOp.call(this, operands);
}
Variable.prototype = Object.create(baseOp.prototype)
Variable.prototype.evaluate = function(...args) {
    if (VARIABLE_NAME.includes(this.operands[0])) {
        return args[VARIABLE_NAME.indexOf(this.operands[0])];
    } else {
        throw new Error("invalid variable name:" + item);
    }
}
Variable.prototype.toString = function() {
    return this.operands[0];
}

function Const(...operands) {
    baseOp.call(this, operands);
}
Const.prototype = Object.create(baseOp.prototype)
Const.prototype.evaluate = function(...args) {
    return this.operands[0];
}
Const.prototype.toString = function() {
    return this.operands[0];
}

function Add(...operands) {
    baseOp.call(this, operands, "+");
}
Add.prototype = Object.create(baseOp.prototype)
Add.prototype.evaluate = function(...args) {
    let evExpr =  baseOp.prototype.evaluate.call(this, ...args);
    return evExpr[0] + evExpr[1];
}

function Subtract(...operands) {
    baseOp.call(this, operands, "-");
}
Subtract.prototype = Object.create(baseOp.prototype)
Subtract.prototype.evaluate = function(...args) {
    let evExpr =  baseOp.prototype.evaluate.call(this, ...args);
    return evExpr[0] - evExpr[1];
}

function Divide(...operands) {
    baseOp.call(this, operands, "/");
}
Divide.prototype = Object.create(baseOp.prototype)
Divide.prototype.evaluate = function(...args) {
    let evExpr =  baseOp.prototype.evaluate.call(this, ...args);
    return evExpr[0] / evExpr[1];
}

function Multiply(...operands) {
    baseOp.call(this, operands, "*");
}
Multiply.prototype = Object.create(baseOp.prototype)
Multiply.prototype.evaluate = function(...args) {
    let evExpr =  baseOp.prototype.evaluate.call(this, ...args);
    return evExpr[0] * evExpr[1];
}

function Negate(...operands) {
    baseOp.call(this, operands, "negate");
}
Negate.prototype = Object.create(baseOp.prototype)
Negate.prototype.evaluate = function(...args) {
    let evExpr =  baseOp.prototype.evaluate.call(this, ...args);
    return -evExpr[0];
}


const OP_NAME = new Map([
    ["negate", [1, Negate]],
    ["+", [2, Add]],
    ["-", [2, Subtract]],
    ["*", [2, Multiply]],
    ["/", [2, Divide]]
]);

let isNumeric = n => !isNaN(n);
let getOperands = (stack, count) => {
    let operands = [];
    for (let i = 0; i !== count; i++) {
        operands.unshift(stack.shift());
    }
    return operands;
}

let parse = (expr) => {
    let mas = expr.split(' ');
    let exprStack = [];
    for (const item of mas) {
        if (item === "") {
            continue;
        }
        if (OP_NAME.has(item)) {
            let ident = OP_NAME.get(item);
            let operands = getOperands(exprStack, ident[0]);
            exprStack.unshift(new ident[1](...operands));
        } else if (VARIABLE_NAME.includes(item)) {
            exprStack.unshift(new Variable(item));
        } else if (isNumeric(item)) {
            exprStack.unshift(new Const(parseInt(item)));
        } else {
            throw new Error("invalid symbol:" + item);
        }
    }
    return exprStack.shift();
}

let expr = new Add(new Const(1), new Variable("x"));
expr.evaluate(1, 2, 3);