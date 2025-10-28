/* eslint-disable @typescript-eslint/no-unused-vars */
import { expect, test, assert } from './test';
import { float2x2, float3x3, float2, float3, float4x4, float4 } from '../dist/vector';
import {polynomial, polynomialT, aberth, polynomialN, legendreTable, aberthT} from '../dist/polynomial';
import complex, { complexT } from '../dist/complex';
import {rational} from '../dist/rational';
import * as big from '../../big/dist/index';

function approxArray(a: number[], b: number[], tol = 1e-9) {
    if (a.length !== b.length)
        return false;
    for (let i = 0; i < a.length; ++i) {
        if (Math.abs(a[i] - b[i]) > tol)
            return false;
    }
    return true;
}

function approx(a: number, b: number, tol = 1e-8) {
	return Math.abs(a - b) <= tol;
}

test('faddeevLeVerrier - identity 3x3', () => {
    const I3 = float3x3.identity();
	const p = I3.characteristic();
	const expectCoeffs = [1, -3, 3, -1];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('faddeevLeVerrier - diagonal 2x2', () => {
    // diag(2,3) => polynomial λ^2 - 5λ + 6
    const D = float2x2(float2(2, 0), float2(0, 3));
	const p = D.characteristic();
	const expectCoeffs = [1, -5, 6];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('faddeevLeVerrier - 2x2 example', () => {
    // matrix [[1,2],[3,4]] with columns (1,3) and (2,4)
    const M = float2x2(float2(1, 3), float2(2, 4));
	const p = M.characteristic();
	// characteristic polynomial: λ^2 - 5λ -2
	const expectCoeffs = [1, -5, -2];
	const coeffs = [1, ...p.c.slice().reverse()];
	assert(approxArray(coeffs, expectCoeffs), `expected ${expectCoeffs}, got ${coeffs}`);
});

test('eigen_2x2 - real distinct', () => {
	const A = float2x2(float2(3, 1), float2(0, 2));
	const eigs = A.eigenvalues();
	if (eigs.length !== 2) throw new Error('expected 2 eigenvalues');
	const vals = eigs.map(e => e.r).sort((a,b) => a-b);
	if (!(approx(vals[0],2) && approx(vals[1],3)))
		throw new Error('real distinct eigenvalues mismatch: ' + JSON.stringify(vals));
});

test('eigen_2x2 - complex conjugate', () => {
	const A = float2x2(float2(0, 2), float2(-2, 0));
	const eigs = A.eigenvalues();
	if (eigs.length !== 2) throw new Error('expected 2 eigenvalues');
	const has2i = eigs.some(e => Math.abs(e.r) < 1e-9 && Math.abs(e.i - 2) < 1e-9);
	const hasMinus2i = eigs.some(e => Math.abs(e.r) < 1e-9 && Math.abs(e.i + 2) < 1e-9);
	if (!has2i || !hasMinus2i) throw new Error('complex conjugate eigenvalues mismatch: ' + JSON.stringify(eigs));
});

test('eigen_3x3 - diag 1,2,3', () => {
	const A = float3x3(float3(1,0,0), float3(0,2,0), float3(0,0,3));
	const eigs = A.eigenvalues();
	if (eigs.length !== 3) throw new Error('expected 3 eigenvalues');
	const vals = eigs.map(e => e.r ?? 0).sort((a,b) => a-b);
	if (!(approx(vals[0],1) && approx(vals[1],2) && approx(vals[2],3)))
		throw new Error('3x3 diagonal eigenvalues mismatch: ' + JSON.stringify(vals));
});

test('eigen_4x4 - diag 1..4', () => {
	const A = float4x4(float4(1,0,0,0), float4(0,2,0,0), float4(0,0,3,0), float4(0,0,0,4));
	const eigs = A.eigenvalues();
	if (eigs.length !== 4) throw new Error('expected 4 eigenvalues');
	const vals = eigs.map(e => e.r).sort((a,b) => a-b);
	if (!(approx(vals[0],1) && approx(vals[1],2) && approx(vals[2],3) && approx(vals[3],4)))
		throw new Error('4x4 diagonal eigenvalues mismatch: ' + JSON.stringify(vals));
});

console.log('testpoly finished');

const a = new rational(3,4);
const b = new rational(2,3);

/*

test('rational arithmetic', () => {
	expect(a.add(b).toString(), '3/4 + 2/3').toEqual('17 / 12');
	expect(a.sub(b).toString(), '3/4 - 2/3').toEqual('1 / 12');
	expect(a.mul(b).toString(), '3/4 * 2/3').toEqual('1 / 2');
	expect(a.div(b).toString(), '3/4 / 2/3').toEqual('9 / 8');//or 1 + 1/8
	expect(a.mod(b).toString(), '3/4 % 2/3').toEqual('1 / 12');//1/8 * 2/3
});
*/
/*
for (let j = 5;  j < 30; ++j) {
	let poly_roots_test = new polynomialT([rational.from(1)]);
	for (let i = 1; i <= j; i++) {
		poly_roots_test = poly_roots_test.mul(new polynomialT([rational.from(-i), rational.from(1)]));
	}
	console.log(poly_roots_test.realRoots().map(r=>+r));
}
*/
/*
console.log(big.float.from(-120).div(1).toString());

for (let j = 5; j < 30; ++j) {
	let poly_roots_test = new polynomialT([big.float.from(1)]);
	for (let i = 1; i <= j; i++) {
		poly_roots_test = poly_roots_test.mul(new polynomialT([big.float.from(-i), big.float.from(1)]));
	}
	console.log(poly_roots_test.realRoots().map(r=>+r));
}
*/
/*
// Explicit test for real roots of (x-1)(x-2)(x-3)(x-4)(x-5)
const poly_roots_test = new polynomial([-1, 1])
	.mul(new polynomial([-2, 1]))
	.mul(new polynomial([-3, 1]))
	.mul(new polynomial([-4, 1]))
	.mul(new polynomial([-5, 1]));
const expected_roots = [1, 2, 3, 4, 5];
const computed_roots = poly_roots_test.realRoots();
console.log('Expected roots:', expected_roots);
console.log('Computed roots:', computed_roots);
if (computed_roots.length !== expected_roots.length || !computed_roots.every((r, i) => Math.abs(r - expected_roots[i]) < 1e-8)) {
	console.error('Root-finding test FAILED for (x-1)...(x-5)');
} else {
	console.log('Root-finding test PASSED for (x-1)...(x-5)');
}
	*/
/*
const table5 = legendreTable(5);
console.log('Legendre P5 coefficients:', table5);

const npoly = new polynomial([-1, 0, 1]); // x^2 - 1
const npoly2 = npoly.mul(npoly);
const nroots = aberth(npoly2.normalise());


const bpoly = new polynomialT<big.float2>([big.float2.from(-1,32), big.float2.from(0,32), big.float2.from(1,32)]); // x^2 - 1
const bpoly2 = bpoly.mul(bpoly);
const broots = aberthT(bpoly2.normalise());

const cpoly = new polynomialT<complex>([complex(1, 1), complex(0, 0), complex(-1, -1)]); // x^2 - 1
console.log('cpoly:', cpoly.toString());
const cpoly2 = cpoly.mul(cpoly);
const roots = aberth(cpoly2.normalise());

const c = complex(1,2);
const c2 = complex.ln(c);

//const poly2 = new polynomial([-4, 0, 1]);
const poly2 = new polynomial([-1, 1]).mul(new polynomial([-2, 1])).mul(new polynomial([-3, 1])).mul(new polynomial([-4, 1])).mul(new polynomial([-5, 1]));
const poly3 = new polynomialN([-1]).mul(new polynomialN([-2])).mul(new polynomialN([-3])).mul(new polynomialN([-4])).mul(new polynomialN([-5])).mul(new polynomialN([-6]));
const r = poly2.realRoots();
console.log(r);

const r1 = poly3.roots();
console.log(r1);

const r2 = aberth(poly2.normalise());
console.log(r2.join('\n'));

const r3 = aberth(poly3);
console.log(r3.join('\n'));
*/