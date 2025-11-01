/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approx, approxArray, makeApproxArray, sequence } from './test';
import {polynomial, polynomialT, polynomialN, legendreTable, polynomialB} from '../dist/polynomial';
import complex, { complexT } from '../dist/complex';
import {rational, rationalB} from '../dist/rational';
import * as big from '../../big/dist/index';

function toleranceForDegree(deg: number, offset = -55, scale = 2.5) {
	return Math.pow(2, deg * scale + offset);
}

test('roots', () => {
	let poly = new polynomial([1]);
	for (let j = 1;  j < 20; ++j) {
		poly = poly.mul(new polynomial([-j, 1]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		verify(roots, sequence(j, 1), makeApproxArray(toleranceForDegree(j)));
	}
});

test('roots of rational', () => {
	let poly = new polynomialT([rational.from(1)]);
	for (let j = 1;  j < 10; ++j) {
		poly = poly.mul(new polynomialT([rational.from(-j), rational.from(1)]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('roots of big rational', () => {
	let poly = new polynomialT([rationalB.from(1)]);
	for (let j = 1;  j < 6; ++j) {
		poly = poly.mul(new polynomialT([rationalB.from(-j), rationalB.from(1)]));
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('roots of big', () => {
	let poly = new polynomialT([big.float.from(1)]);
	let rpoly = new polynomialT([rational.from(1)]);
	for (let j = 1; j <= 8; ++j) {
		poly = poly.mul(new polynomialT([big.float.from(-j, 128).div(10), big.float.from(1)]));
		rpoly = rpoly.mul(new polynomialT([rational.from(-j).div(rational.from(10)), rational.from(1)]));
	}
	const rroots0 = rpoly.rationalRoots();
	const rroots1 = poly.rationalRoots();

	poly = new polynomialT([big.float.from(1)]);

	for (let j = 1; j < 10; ++j) {
		poly = poly.mul(new polynomialT([big.float.from(-j, 128).div(10), big.float.from(1)]));
		const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		const expected = sequence(j, 0.1, 0.1);
		verify(rroots!.map(r=>Number(r)), expected, makeApproxArray(2e-7));
		verify(roots.map(r=>+r), expected, makeApproxArray(2e-7));
	}
});

test('rational roots', () => {
	let poly = new polynomialB([1n]);
	for (let j = 1; j < 20; ++j) {
		poly = poly.mul(new polynomialB([BigInt(-j), 8n]));
		const roots = poly.rationalRoots();
		verify(roots.map(r=>Number(r)), sequence(j, 0.125, 0.125), makeApproxArray(2e-7));
	}
});

