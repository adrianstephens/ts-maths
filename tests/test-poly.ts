/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert, verify, approxArray, makeApproxArray, sequence } from './test';
import { Polynomial, PolynomialN, legendreTable } from '../dist/polynomial';
import {rational, rationalB} from '../dist/rational';
import big from '../../big/dist/index';

function toleranceForDegree(deg: number, offset = -55, scale = 2.5) {
	return Math.pow(2, deg * scale + offset);
}


test('sparse', () => {
	function makePoly(i: number, gap: number) {
		return Polynomial([-i, ...Array.from({length: gap - 1}, () => 0), 1]);
	}
	function testGap(gap: number) {
		let poly = Polynomial([1]);
		for (let j = 1; j < 5; ++j) {
			poly = poly.mul(makePoly(j, gap));
			console.log(String(poly));
			const roots = poly.realRoots();
			const expected = gap % 2 ? sequence(j, 1) : [...sequence(j, -1, -1).reverse(), ...sequence(j, 1)];
			verify(roots.map(r=>+r), expected, makeApproxArray(2e-7));
		}
	}
	testGap(2);
	testGap(3);
	testGap(4);
	testGap(10);
});

test('roots', () => {
	let poly = Polynomial([1]);
	for (let j = 1;  j < 20; ++j) {
		poly = poly.mul(Polynomial([-j, 1]));
		console.log(String(poly));
		//const rroots = poly.rationalRoots();
		const roots = poly.realRoots();
		verify(roots, sequence(j, 1), makeApproxArray(toleranceForDegree(j)));
	}
});

test('roots of rational', () => {
	let poly = Polynomial([rational(1)]);
	for (let j = 1;  j < 10; ++j) {
		poly = poly.mul(Polynomial([rational(-j), rational(1)]));
		//const rroots = poly.rationalRoots();
		console.log(String(poly));
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('roots of big rational', () => {
	let poly = Polynomial([rationalB.from(1)]);
	for (let j = 1;  j < 6; ++j) {
		poly = poly.mul(Polynomial([rationalB.from(-j), rationalB.from(1)]));
		console.log(String(poly));
		const roots = poly.realRoots();
		verify(roots.map(r=>+r), sequence(j, 1), makeApproxArray(2e-7));
	}
});

test('rational roots', () => {
	let poly = Polynomial([1n]);
	for (let j = 1; j < 20; ++j) {
		poly = poly.mul(Polynomial([BigInt(-j), 8n]));
		console.log(String(poly));
		const roots = poly.rationalRoots();
		verify(roots.map(r=>Number(r)), sequence(j, 0.125, 0.125), makeApproxArray(2e-7));
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



