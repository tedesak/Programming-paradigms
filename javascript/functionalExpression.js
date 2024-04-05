"use strict"
const OP_NAME = new Map([["+", add], ["-", subtract], ["*", multiply], ["/", divide]]);
const VARIABLE_NAME = new Set(["x", "y", "z"]);
let calc = (f, f1, f2) => (a, b, c) => {
    let val1 = typeof(f1) === "function" ? f1(a, b, c) : f1;
    let val2 = typeof(f2) === "function" ? f2(a, b, c) : f2;
    return f(val1, val2, a, b, c);
}
let baseOp = (f) => (f1, f2) => {
    return calc(f, f1, f2);
}
let add = baseOp((a, b) => (a + b));
let subtract = baseOp((a, b) => (a - b));
let multiply = baseOp((a, b) => (a * b));
let divide = baseOp((a, b) => (a / b));
let negate = baseOp((a) => (-a));
let cnst = baseOp((a) => a);
let variable = baseOp((val1, _val2, a, b, c) => {
    if (val1 === "x") {
        return a;
    } else if (val1 === "y") {
        return b;
    } else if (val1 === "z") {
        return c;
    } else {
        return NaN;
    }
});

let isNumeric = n => !isNaN(n)

let parse = (expr) => {
    let mas = expr.split(' ');
    let exprStack = [];
    for (const item of mas) {
        if (OP_NAME.has(item)) {
            let operand1 = exprStack.shift();
            let operand2 = exprStack.shift();
            exprStack.unshift(OP_NAME.get(item)(operand2, operand1));
        } else if (VARIABLE_NAME.has(item)) {
            exprStack.unshift(variable(item));
        } else if (isNumeric(item)) {
            exprStack.unshift(cnst(parseInt(item)));
        } else {
            console.log("something is wrong");
        }
    }
    return exprStack.shift();
}
/*
let expr = add(
    subtract(
        multiply(
            variable("x"),
            variable("x")
        ),
        multiply(
            cnst(2),
            variable("x")
    )),
    cnst(1)
);
*/