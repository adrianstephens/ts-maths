/* eslint-disable @typescript-eslint/no-unused-vars */
import { test, expect, approx } from './test';
import { symbolic, applyRules, trigRules, invTrigRules } from '../dist/symbolic';
import { applyRulesEgraph } from '../dist/egraph';
import { polynomialT } from '../dist/polynomial';

function makeFunction(expr: symbolic, varName: string) {
	return (x: number) => expr.evaluate({ [varName]: x });
}

function makeDeriv(f: (x: number) => number, h = 1e-6) {
	return (x: number) => (f(x + h) - f(x - h)) / (2 * h);
}

function checkDeriv(expr: symbolic, varName: string, expected: (x: number) => number, testPoints: number[] = [0, 0.5, 1, 2, 3, 5]) {
	const deriv = expr.derivative(varName);
	const d = makeFunction(deriv, varName);
	for (const x of testPoints)
		expect(d(x)).toBeCloseTo(expected(x));
}


function checkDeriv2(expr: symbolic, varName: string, testPoints: number[] = [0], h = 1e-6) {
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

test('symbolic', () => {
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
	expect(dsin.toString()).toEqual('2 * cos(x² + y² + z²) * x');
	expect(dsin).toEqual(symbolic.cos(e3).mul(x).mul(2));

	const pow = e3.pow(x);
	const dpow = pow.derivative("x");
	expect(dpow.toString()).toEqual('2 * x² * (x² + y² + z²) ^ (x - 1) + log(x² + y² + z²) * (x² + y² + z²) ^ (x)');
});

test('symbolic transforms', () => {
    const x = symbolic.variable("x");
    const y = symbolic.variable("y");
    const sum = x.add(y);

	const sindif = applyRules(symbolic.sin(x.sub(y)), trigRules);
	expect(sindif).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).sub(symbolic.cos(x).mul(symbolic.sin(y))));
	const sinsum = applyRules(symbolic.sin(sum), trigRules);
	expect(sinsum).toEqual(symbolic.sin(x).mul(symbolic.cos(y)).add(symbolic.cos(x).mul(symbolic.sin(y))));

//	const esinsum = applyRulesEgraph(symbolic.sin(sum), trigRules, node => 0);

	const sinsum2 = applyRules(sinsum, invTrigRules);
	expect(sinsum2).toEqual(symbolic.sin(sum));

	const sinhalf0 = symbolic.sin(sum.scale(0.5));
	const sinhalf1 = symbolic.sin(x.scale(0.5));

	const sinhalf = applyRules(sinhalf0, trigRules);
	expect(sinhalf).toEqual((symbolic.sin(x).mul(symbolic.sin(y)).sub(symbolic.cos(x).mul(symbolic.cos(y))).add(1)).pow(0.5).scale(Math.sqrt(0.5)));
});

test('symbolic instance adapter basic', () => {
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

	checkDeriv2(expr, 'x', [3]);
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

    // (x+1)^2 -> x^2 + 2*x + 1
    expect(expanded).toEqual(x.pow(2).add(x.mul(2)).add(1));
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



