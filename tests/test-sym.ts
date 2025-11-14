/* eslint-disable @typescript-eslint/no-unused-vars */
import { test, expect, approx, assert } from './test';
import { OperatorsBase, numberOperators} from '../dist/core';
import { parseNumber, outputNumber, toSuperscript, parse } from '../dist/string';
import { symbolic, symbolicOperators, applyRules, trigRules, invTrigRules, generalRules, scoreFactory } from '../dist/symbolic';
import { applyRulesEgraph } from '../dist/egraph';
import { polynomialT } from '../dist/polynomial';
import { E2, vectorT } from '../dist/vector';
import * as big from '../../big/dist/index';

class BigOperators extends OperatorsBase<big.float> {
	from(n: number): big.float		{ return big.float.from(n); }
	func(name: string, args: big.float[]) {
		switch (name) {
			case 'sin': 		return big.sin(args[0], 100);
			case 'cos': 		return big.cos(args[0], 100);
			case 'tan': 		return big.tan(args[0], 100);
			case 'asin': 		return big.asin(args[0], 100);
			case 'acos': 		return big.acos(args[0], 100);
			case 'atan': 		return big.atan(args[0], 100);
			case 'atan2': 		return big.atan2(args[0], args[1], 100);
			case 'exp': 		return big.exp(args[0], 100);
			case 'log': 		return big.log(args[0], 100);
			case 'sqrt': 		return args[0].sqrt();
			default:			return undefined;
		}
	}
	variable(name: string) {
		switch (name) {
			case 'pi':			return big.pi(100);
			case 'e':			return big.exp(1, 100);
			case 'infinity':	return big.float.Infinity;
			default:			return undefined;
		}
	}
	pow(a: big.float, b: big.float): big.float {
		return a.pow(b);
	}
};
const bigOperators = new BigOperators();


function makeFunction(expr: symbolic, var1: string) {
	return (x: number) => expr.evaluate({ [var1]: x });
}

function makeDeriv(f: (x: number) => number, h = 1e-6) {
	return (x: number) => (f(x + h) - f(x - h)) / (2 * h);
}

function checkDeriv(expr: symbolic, varName: string, testPoints = [0], h = 1e-6) {
	const deriv = expr.derivative(varName);
	const D0 = makeFunction(deriv, varName);
	const D1 = makeDeriv(makeFunction(expr, varName), h);

	for (const x of testPoints) {
		const d0 = D0(x);
		const d1 = D1(x);
		if (Math.abs(d1 - d0) > 1e-6)
			return false;
	}
	return true;
}

function approxEqualEval(x: symbolic, y: symbolic, varNames: string[], testPoints: Iterable<number[]> = [[0.1], [0.5], [1.2], [-0.7]], h = 1e-6) {
	for (const v of testPoints) {
		const env = Object.fromEntries(varNames.map((name, i) => [name, v[i % v.length]]));
		const xv = x.evaluate(env);
		const yv = y.evaluate(env);
		if (Math.abs(xv - yv) > 1e-8)
			return false;
	}
	return true;
}

function countNodes(n: symbolic) {
	let cnt = 0;
	n.visit({post: x => { cnt++; return x; }});
	return cnt;
}

test('symbolic', () => {
	const equation = 'sin(pi / 4) + log(10) sqrt(2)';
	const resn = parse(numberOperators, equation);
	const resb = parse(bigOperators, equation);
	expect(resn).toBeCloseTo(Math.sin(Math.PI / 4) + Math.log(10) * Math.sqrt(2));

	expect(parseNumber('∛⅒')[1]).toBeCloseTo(0.1 ** (1 / 3));
	expect(outputNumber(0.1 ** (1 / 3))).toEqual('∛⅒');
	expect(parseNumber('⁶⁷/₂₃₄')[1]).toBeCloseTo(67/234);
	expect(parseNumber(toSuperscript('67/234'))[1]).toBeCloseTo(67/234);
	expect(parseNumber(toSuperscript((67/234).toString()))[1]).toBeCloseTo(67/234);

	const x = symbolic.variable("x");
	const y = symbolic.variable("y");
	const z = symbolic.variable("z");

	const sum = x.add(y);
	const sum2 = sum.add(z);
	expect(sum.toString()).toEqual('x + y');
	expect(sum2.toString()).toEqual('x + y + z');

	const e2 = sum.div(sum2);
	const d2 = e2.derivative("x").factor();
	expect(d2).toEqual(z.div(sum2.pow(2)));

	const j = symbolic.i.scale(3).add(2);
	const j2 = j.mul(j).expand();
	expect(j2.toString()).toEqual('12 * 𝑖 - 5');

	const e3 = x.pow(2).add(y.pow(2)).add(z.pow(2));

	const sin = symbolic.sin(e3);
	const dsin = sin.derivative("x");
	expect(dsin).toEqual(parse(symbolicOperators, '2 x cos(x² + y² + z²)'));

	const pow = e3.pow(x);
	const dpow = pow.derivative("x");
	expect(dpow).toEqual(parse(symbolicOperators, '2x²(x² + y² + z²)^(x - 1) + log(x² + y² + z²)(x² + y² + z²)^x'));
	expect(dpow).toEqual(parse(symbolicOperators, '2x.x(x² + y² + z²)^(x - 1) + log(x² + y² + z²)(x² + y² + z²)^x'));
//	expect(dpow).toEqual(parse(symbolicOperators, '2 * x² * (x² + y² + z²) ^ (x - 1) + log(x² + y² + z²) * (x² + y² + z²) ^ (x)'));
});

test('symbolic transforms', () => {
	// expand sin(a+b)*cos(a-b) into combination using product-to-sum then simplify
	const a = symbolic.variable('a');
	const b = symbolic.variable('b');

	const test = parse(symbolicOperators, '(cos(a) * cos(b) + sin(a) * sin(b)) * (cos(a) * sin(b) + cos(b) * sin(a))');
	const test2 = test.expand();
	const test3 = test2.factor();
	const bs = generalRules.find(r => r.name === 'factor-common')?.match(test2);
		

	// sin(a+b)*cos(a-b) = 1/2[sin(2a) + sin(2b)] (using trig identities)
	const expr = symbolic.sin(a.add(b)).mul(symbolic.cos(a.sub(b)));

	// Apply egraph with available rules (symbolic exposes a list of rules)
	const rules = trigRules.concat(invTrigRules).concat(generalRules);
	const out = applyRulesEgraph(expr, rules);//, {rounds: 100, verbose: true, debugNode: 'replace'});

	console.log('Original  :', String(expr));
	console.log('Extracted :', String(out));
	if (!approxEqualEval(out, expr, ['a', 'b']))
		throw new Error('egraph extraction is not equivalent to original expression');

	const x = symbolic.variable("x");
	const y = symbolic.variable("y");
	const sum = x.add(y);

	const sindif = applyRules(symbolic.sin(x.sub(y)), trigRules);
	expect(sindif).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).sub(symbolic.cos(x).mul(symbolic.sin(y))));
	const sinsum = symbolic.sin(sum);
	const sinsum1 = applyRules(sinsum, trigRules);
	expect(sinsum1).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).add(symbolic.cos(x).mul(symbolic.sin(y))));

	const esinsum = applyRulesEgraph(sinsum, trigRules, {scorer: node => -countNodes(node), verbose: false});
	expect(esinsum).toEqual(sinsum1);

	const sinsum2 = applyRules(sinsum1, invTrigRules);
	// structural equality may vary; assert numeric equivalence at sample points
	if (!approxEqualEval(sinsum2, sinsum, ['x', 'y'], [[0, 0], [0.3, 0.7], [1.2, -0.5]]))
		throw new Error('egraph extraction is not equivalent to original expression');

	const sinhalf = applyRules(symbolic.sin(sum.scale(0.5)), trigRules);
	expect(sinhalf).toEqual((symbolic.sin(x).mul(symbolic.sin(y)).sub(symbolic.cos(x).mul(symbolic.cos(y))).add(1)).pow(0.5).scale(Math.sqrt(0.5)));

});

test('symbolic trig/hyperbolic', () => {
	const x = symbolic.variable("x");
	const y = symbolic.variable("y");

	// additional function checks
	const a_asin = symbolic.asin(x);
	expect(a_asin.evaluate({ x: 0.5 }), 'asin eval').check(v => Math.abs(v - Math.asin(0.5)) < 1e-12);
	const da_asin = a_asin.derivative('x');
	expect(da_asin.evaluate({ x: 0.5 }), 'asin deriv').check(v => Math.abs(v - 1 / Math.sqrt(1 - 0.5 * 0.5)) < 1e-12);

	const a_acos = symbolic.acos(x);
	expect(a_acos.evaluate({ x: 0.5 }), 'acos eval').check(v => Math.abs(v - Math.acos(0.5)) < 1e-12);
	const da_acos = a_acos.derivative('x');
	expect(da_acos.evaluate({ x: 0.5 }), 'acos deriv').check(v => Math.abs(v - (-1 / Math.sqrt(1 - 0.5 * 0.5))) < 1e-12);

	const a_atan = symbolic.atan(x);
	expect(a_atan.evaluate({ x: 1 }), 'atan eval').check(v => Math.abs(v - Math.atan(1)) < 1e-12);
	const da_atan = a_atan.derivative('x');
	expect(da_atan.evaluate({ x: 1 }), 'atan deriv').check(v => Math.abs(v - (1 / (1 + 1 * 1))) < 1e-12);

	const a_atan2 = symbolic.atan2(y, x);
	expect(a_atan2.evaluate({ x: 2, y: 1 }), 'atan2 eval').check(v => Math.abs(v - Math.atan2(1, 2)) < 1e-12);
	const da_atan2_dx = a_atan2.derivative('x');
	// derivative w.r.t x: -y / (x^2 + y^2)
	expect(da_atan2_dx.evaluate({ x: 2, y: 1 }), 'atan2 dx').check(v => Math.abs(v - (-1 / (4 + 1))) < 1e-12);

	const a_sinh = symbolic.sinh(x);
	expect(a_sinh.evaluate({ x: 0.5 }), 'sinh eval').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);
	const da_sinh = a_sinh.derivative('x');
	expect(da_sinh.evaluate({ x: 0.5 }), 'sinh deriv').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);

	const a_cosh = symbolic.cosh(x);
	expect(a_cosh.evaluate({ x: 0.5 }), 'cosh eval').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);
	const da_cosh = a_cosh.derivative('x');
	expect(da_cosh.evaluate({ x: 0.5 }), 'cosh deriv').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);

	const a_tanh = symbolic.tanh(x);
	expect(a_tanh.evaluate({ x: 0.5 }), 'tanh eval').check(v => Math.abs(v - Math.tanh(0.5)) < 1e-12);
	const da_tanh = a_tanh.derivative('x');
	expect(da_tanh.evaluate({ x: 0.5 }), 'tanh deriv').check(v => Math.abs(v - (1 - Math.tanh(0.5) ** 2)) < 1e-12);

});

test('canonicalization merges powers and derivatives simplify', () => {
	const x = symbolic.variable("x");

	const c = x.mul(2).add(x.mul(3));
	const d = x.mul(5);
	expect(c).toEqual(d);

	const a = x.pow(2).mul(x.pow(3));
	const b = x.pow(5);
	expect(a).toEqual(b);

	const da = a.derivative("x");
	expect(da.evaluate({ x: 2 }), 'derivative value').toEqual(5 * Math.pow(2, 4));

	// more complex: x^2 * (x+1) * x^3 -> x^5*(x+1)
	const expr = x.pow(2).mul(x.add(1)).mul(x.pow(3));
	expect(expr).toEqual(x.pow(5).mul(x.add(1)));

	checkDeriv(expr, 'x', [3]);
});

test('expand distributes multiplication over addition (single additive factor)', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	const expr = x.add(1).mul(y);
	const expanded = expr.expand({});

	const expected = x.mul(y).add(y);
	expect(expanded).toEqual(expected);
});

test('expand distributes multiplication over addition (two additive factors)', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	const expr = x.add(1).mul(y.add(2));
	const expanded = expr.expand({});

	// (x+1)*(y+2) -> x*y + 2*x + y + 2
	expect(expanded).toEqual(x.mul(y).add(x.mul(2)).add(y).add(2));
});

test('expand distributes multiplication over powers)', () => {
	const x = symbolic.variable('x');

	const expr = x.add(1).pow(2);
	const expanded = expr.expand({});
	const expected = x.pow(2).add(x.mul(2)).add(1);

	// (x+1)^2 -> x^2 + 2*x + 1
	expect(expanded).toEqual(expected);
});

test('collect groups terms by variable power (no expand)', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	// x^2 * y + 2 * x * y + 3
	const expr = x.pow(2).mul(y).add(x.mul(y).mul(2)).add(3);
	const groups = expr.collect('x');
	const pg = new polynomialT(groups).evaluate(x).expand();

	// groups is a sparse array: index = power, value = coefficient (symbolic)
	expect(pg).toEqual(expr);
});

test('collect after expand (x+1)^2 * y', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	const expr = x.add(1).pow(2).mul(y);
	const expanded = expr.expand({});
	const groups = expanded.collect('x');
	const pg = new polynomialT(groups).evaluate(x).expand();

	expect(pg).toEqual(expanded);
});


test('egraph smoke', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');
	const out = applyRulesEgraph(symbolic.sin(x.add(y)), trigRules, {scorer: node => -countNodes(node)});
	expect(typeof out.id).toEqual('string');
});

test('egraph general expand+collect simplification', () => {
	const x = symbolic.variable('x');
	// expr = (x+1)*(x+2) + (x+1)*x  => should factor to (x+1)*(2x+2)
	const expr = x.add(1).mul(x.add(2)).add(x.add(1).mul(x));
	const out = applyRulesEgraph(expr, generalRules);

	// expected factorised form (x+1)*(x+2 + x) == (x+1)*(2x+2)
	const expected = x.add(1).mul(x.add(2).add(x));

	// assert the extractor chose the expected factored form (structural equality)
	expect(out).toEqual(expected);
});

test('egraph ambitious factorisations (scoreFactory params)', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	// Example 1: three terms sharing (x+1)
	// (x+1)*(x+2) + (x+1)*(x+3) + (x+1)*x  => (x+1)*(3*x + 5)
	const expr1 = x.add(1).mul(x.add(2)).add(x.add(1).mul(x.add(3))).add(x.add(1).mul(x));
	const expected1 = x.add(1).mul(x.mul(3).add(5));

	// Example 2: mixed variables, factor (x+1) across different y-terms
	// (x+1)*(y+2) + (x+1)*(y+3) => (x+1)*(2*y + 5)
	const expr2 = x.add(1).mul(y.add(2)).add(x.add(1).mul(y.add(3)));
	const expected2 = x.add(1).mul(y.mul(2).add(5));

	// Use a conservative recommended scorer (exposed constW) to encourage factoring
	const scorer = scoreFactory(0.2, 0.5, 1, 0.1);

	const out1 = applyRulesEgraph(expr1, generalRules, {scorer});
	const out2 = applyRulesEgraph(expr2, generalRules, {scorer});

	// assert the extractor chose the expected factored forms (structural equality)
	expect(out1).toEqual(expected1);
	expect(out2).toEqual(expected2);
});


test('vector perp()', () => {
	const x = symbolic.variable('x');
	const y = symbolic.variable('y');

	const v = vectorT(E2, x, y);
	const d = v.dot(v);
	console.log('v.dot(v) =', d.toString());
});
