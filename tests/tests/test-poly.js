"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
/* eslint-disable @typescript-eslint/no-unused-vars */
const test_1 = require("./test");
const polynomial_1 = require("../dist/polynomial");
const rational_1 = require("../dist/rational");
const index_1 = __importDefault(require("../../big/dist/index"));
function toleranceForDegree(deg, offset = -55, scale = 2.5) {
    return Math.pow(2, deg * scale + offset);
}
(0, test_1.test)('sparse', () => {
    function makePoly(i, gap) {
        return (0, polynomial_1.Polynomial)([-i, ...Array.from({ length: gap - 1 }, () => 0), 1]);
    }
    function testGap(gap) {
        let poly = (0, polynomial_1.Polynomial)([1]);
        for (let j = 1; j < 5; ++j) {
            poly = poly.mul(makePoly(j, gap));
            console.log(String(poly));
            const roots = poly.realRoots();
            const expected = gap % 2 ? (0, test_1.sequence)(j, 1) : [...(0, test_1.sequence)(j, -1, -1).reverse(), ...(0, test_1.sequence)(j, 1)];
            (0, test_1.verify)(roots.map(r => +r), expected, (0, test_1.makeApproxArray)(2e-7));
        }
    }
    testGap(2);
    testGap(3);
    testGap(4);
    testGap(10);
});
(0, test_1.test)('roots', () => {
    let poly = (0, polynomial_1.Polynomial)([1]);
    for (let j = 1; j < 20; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([-j, 1]));
        console.log(String(poly));
        //const rroots = poly.rationalRoots();
        const roots = poly.realRoots();
        (0, test_1.verify)(roots, (0, test_1.sequence)(j, 1), (0, test_1.makeApproxArray)(toleranceForDegree(j)));
    }
});
(0, test_1.test)('roots of rational', () => {
    let poly = (0, polynomial_1.Polynomial)([(0, rational_1.rational)(1)]);
    for (let j = 1; j < 10; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([(0, rational_1.rational)(-j), (0, rational_1.rational)(1)]));
        //const rroots = poly.rationalRoots();
        console.log(String(poly));
        const roots = poly.realRoots();
        (0, test_1.verify)(roots.map(r => +r), (0, test_1.sequence)(j, 1), (0, test_1.makeApproxArray)(2e-7));
    }
});
(0, test_1.test)('roots of big rational', () => {
    let poly = (0, polynomial_1.Polynomial)([rational_1.rationalB.from(1)]);
    for (let j = 1; j < 6; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([rational_1.rationalB.from(-j), rational_1.rationalB.from(1)]));
        console.log(String(poly));
        const roots = poly.realRoots();
        (0, test_1.verify)(roots.map(r => +r), (0, test_1.sequence)(j, 1), (0, test_1.makeApproxArray)(2e-7));
    }
});
(0, test_1.test)('rational roots', () => {
    let poly = (0, polynomial_1.Polynomial)([1n]);
    for (let j = 1; j < 20; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([BigInt(-j), 8n]));
        console.log(String(poly));
        const roots = poly.rationalRoots();
        (0, test_1.verify)(roots.map(r => Number(r)), (0, test_1.sequence)(j, 0.125, 0.125), (0, test_1.makeApproxArray)(2e-7));
    }
});
(0, test_1.test)('roots of big', () => {
    let poly = (0, polynomial_1.Polynomial)([index_1.default.from(1)]);
    let rpoly = (0, polynomial_1.Polynomial)([rational_1.rational.from(1)]);
    for (let j = 1; j <= 8; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([index_1.default.from(-j, 128).div(10), index_1.default.from(1)]));
        rpoly = rpoly.mul((0, polynomial_1.Polynomial)([rational_1.rational.from(-j).div(rational_1.rational.from(10)), rational_1.rational.from(1)]));
    }
    const rroots0 = rpoly.rationalRoots();
    const rroots1 = poly.rationalRoots();
    poly = (0, polynomial_1.Polynomial)([index_1.default.from(1)]);
    for (let j = 1; j < 10; ++j) {
        poly = poly.mul((0, polynomial_1.Polynomial)([index_1.default.from(-j, 128).div(10), index_1.default.from(1)]));
        const rroots = poly.rationalRoots();
        const roots = poly.realRoots();
        const expected = (0, test_1.sequence)(j, 0.1, 0.1);
        (0, test_1.verify)(rroots.map(r => Number(r)), expected, (0, test_1.makeApproxArray)(2e-7));
        (0, test_1.verify)(roots.map(r => +r), expected, (0, test_1.makeApproxArray)(2e-7));
    }
});
//# sourceMappingURL=test-poly.js.map