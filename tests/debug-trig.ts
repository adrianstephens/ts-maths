import { symbolic, applyRules, trigRules, type Rule } from '../src/symbolic';
import type { symbolic as symbolicType } from '../src/symbolic';

function printStage(name: string, node: symbolicType) {
    console.log(`--- ${name} ---`);
    try { console.log(node.toString({ printConst: (n: number) => n.toString() })); } catch { console.log(String(node)); }
}

function approxEqual(a: number, b: number, eps = 1e-9) {
    return Math.abs(a - b) < eps;
}

function numericCheck(orig: symbolicType, transformed: symbolicType) {
    for (const [va, vb] of [[0.3, 1.1], [0.7, 2.2], [1.2, 0.4]]) {
        const val1 = orig.evaluate({ a: va, b: vb });
        const val2 = transformed.evaluate({ a: va, b: vb });
        console.log(`a=${va}, b=${vb} -> orig=${val1}, new=${val2}`);
        if (!approxEqual(val1, val2, 1e-6))
            return false;
    }
    return true;
}

const a = symbolic.variable('a');
const b = symbolic.variable('b');

const term1 = symbolic.cos(b).pow(2).mul(symbolic.cos(a).mul(symbolic.sin(a)));
const term2 = symbolic.sin(b).pow(2).mul(symbolic.cos(a).mul(symbolic.sin(a)));
const term3 = symbolic.cos(a).pow(2).mul(symbolic.cos(b).mul(symbolic.sin(b)));
const term4 = symbolic.sin(a).pow(2).mul(symbolic.cos(b).mul(symbolic.sin(b)));

const expr = term1.add(term2).add(term3).add(term4);

printStage('original', expr);

const factored = expr.factor();
printStage('factored', factored);

const pythag = trigRules.find(r => r.name === 'sin-cos-pythag');
const doubleAngle = trigRules.find(r => r.name === 'double-angle-sin-compress');
const selectedRules = [pythag, doubleAngle].filter((r): r is Rule | undefined => Boolean(r)) as Rule[];
const rewritten = applyRules(factored, selectedRules);
printStage('rewritten', rewritten);

console.log('numeric check:', numericCheck(expr, rewritten));
