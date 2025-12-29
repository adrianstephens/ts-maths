"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.expect = expect;
exports.test = test;
exports.verify = verify;
exports.assert = assert;
exports.approxArray = approxArray;
exports.makeApproxArray = makeApproxArray;
exports.sequence = sequence;
const real_1 = __importDefault(require("../dist/real"));
function expect(v, description) {
    return {
        toEqual(v2) {
            const success = typeof v === 'object' && v ? v.eq(v2) : v === v2;
            console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}${v} === ${v2}`);
            //if (!success)
            //	console.log(`fail: expected ${v2}, got ${v}`);
        },
        toBeCloseTo(v2, tol = 1e-8) {
            const success = typeof v === 'object' && v ? v.approx?.(v2, tol) : real_1.default.approx(v, v2, tol);
            console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}${v} ≈ ${v2}`);
        },
        check(test) {
            const success = test(v);
            console.log(`${success ? '✓' : '✗'}: ${description ?? ''}`);
        },
    };
}
function test(name, fn) {
    console.log('---------------------');
    console.log("testing: " + name);
    fn();
    console.log("finished: " + name);
}
function verify(v1, v2, test, description) {
    const success = test(v1, v2);
    console.log(`${success ? '✓' : '✗'}: ${description ? description + ' ' : ''}expected ${v2}, got ${v1}`);
}
function assert(condition, msg) {
    if (!condition)
        throw new Error(msg || 'assertion failed');
}
function approxArray(a, b, tol = 1e-9) {
    if (a.length !== b.length)
        return false;
    for (let i = 0; i < a.length; ++i) {
        if (Math.abs(a[i] - b[i]) > tol)
            return false;
    }
    return true;
}
function makeApproxArray(tol) {
    return (a, b) => approxArray(a, b, tol);
}
function sequence(length, from = 0, step = 1) {
    return Array.from({ length }, (_, k) => from + k * step);
}
//# sourceMappingURL=test.js.map