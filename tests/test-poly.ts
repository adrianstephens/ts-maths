/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approxArray, makeApproxArray, sequence } from './test';
import { Operators, OperatorsBase, approx } from '../dist/core';
import { Polynomial, PolynomialN, legendreTable } from '../dist/polynomial';
import { symbolic } from '../dist/symbolic';
import { generalRules } from '../dist/symbolicRules';
import complex, { _complex, complexT, complexOps } from '../dist/complex';
import {rational, rationalB} from '../dist/rational';
import big from '../../big/dist/index';
import { outputNumber, radicalChars, fractionChars } from '../dist/string';
import EGraph, { applyRulesEgraph } from '../dist/egraph';

symbolic.setDefaultStringifyOptions({
	superPower: true,
	radicalPower: true,
	mulChar: '⋅',
	printConst: n=>outputNumber(n, {fractions: {chars: fractionChars, superSub: true}, radicals: radicalChars})}
);

function toleranceForDegree(deg: number, offset = -55, scale = 2.5) {
	return Math.pow(2, deg * scale + offset);
}


test('symbolic polynomials', () => {
	let poly = Polynomial<symbolic>([symbolic.from(1)]);
	const bindings = {
		A: complex(1),
		B: complex(2),
		C: complex(3),
		D: complex(4),
		E: complex(5),
		F: complex(6),
		G: complex(7),
		H: complex(8),
	};
 
	function validate(a: symbolic, b: symbolic) {
		const va = a.evaluateT(complexOps, bindings);
		const vb = b.evaluateT(complexOps, bindings);
		if (va && vb && (va.approx(vb, 1e-10) || va.approx(vb.neg(), 1e-10)))
			return true;
		return false;
	}

	for (let j = 1; j < 8; ++j) {
		poly = poly.mul(Polynomial<symbolic>([symbolic.variable('ABCDEFGH'[j  - 1]).neg(), symbolic.from(1)]));
		console.log(`degree ${j} polynomial: ${poly.toString()}`);
		const roots = poly.realRoots();
		console.log(roots.map(String).join('\n'));
		console.log('simplifying:');
		for (const r of roots) {
			const timeoutMs = 20000;
			const start = Date.now();
			const callback = () => {
				if (Date.now() - start > timeoutMs) {
					console.warn(`egraph-diagnostic: timeout of ${timeoutMs}ms`);
					return false;
				}
				return true;
			};

			const r2 = applyRulesEgraph(r, generalRules, {verbose: false, debugNode: 'replace'/*, callback*/, validate});
			console.log(String(r2));
		}
	}
});

test('roots', () => {
	let poly = Polynomial([1]);
	for (let j = 1;  j < 20; ++j) {
		poly = poly.mul(Polynomial([-j, 1]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		verify(roots, sequence(j, 1), makeApproxArray(toleranceForDegree(j)));
	}
});

test('roots of rational', () => {
	let poly = Polynomial([rational.from(1)]);
	for (let j = 1;  j < 10; ++j) {
		poly = poly.mul(Polynomial([rational.from(-j), rational.from(1)]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('roots of big rational', () => {
	let poly = Polynomial([rationalB.from(1)]);
	for (let j = 1;  j < 6; ++j) {
		poly = poly.mul(Polynomial([rationalB.from(-j), rationalB.from(1)]));
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('roots of big', () => {
	let poly = Polynomial([big.from(1)]);
	let rpoly = Polynomial([rational.from(1)]);
	for (let j = 1; j <= 8; ++j) {
		poly = poly.mul(Polynomial([big.from(-j, 128).div(10), big.from(1)]));
		rpoly = rpoly.mul(Polynomial([rational.from(-j).div(rational.from(10)), rational.from(1)]));
	}
	const rroots0 = rpoly.rationalRoots();
	const rroots1 = poly.rationalRoots();

	poly = Polynomial([big.from(1)]);

	for (let j = 1; j < 10; ++j) {
		poly = poly.mul(Polynomial([big.from(-j, 128).div(10), big.from(1)]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		const expected = sequence(j, 0.1, 0.1);
		verify(rroots!.map(r=>Number(r)), expected, makeApproxArray(2e-7));
		verify(roots.map(r=>+r), expected, makeApproxArray(2e-7));
	}
});

test('rational roots', () => {
	let poly = Polynomial([1n]);
	for (let j = 1; j < 20; ++j) {
		poly = poly.mul(Polynomial([BigInt(-j), 8n]));
		const roots = poly.rationalRoots();
		verify(roots.map(r=>Number(r)), sequence(j, 0.125, 0.125), makeApproxArray(2e-7));
	}
});


