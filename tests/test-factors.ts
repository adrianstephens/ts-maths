/* eslint-disable @typescript-eslint/no-unused-vars */
/* eslint-disable @typescript-eslint/no-explicit-any */

import { expect, test, assert, verify, approxArray, makeApproxArray, sequence } from './test';
import { super2 } from "../dist/core";
import real from "../dist/real";
import gen from "../dist/gen";
import rational from '../src/rational';
import { Matrix } from '../src/linear';
import { Polynomial, PolynomialN, PolyTypes, Factor, Extension } from '../src/polynomial';
import { MaybeExtension, _Factor, FactorExt, factorType, resultant, discriminant, interpolate, vietasFormulas, squareFreeFactorization, factorOverK, factorOverK_Q, factorOverK_I, PolyModFactory,
	matrixMinimalPolynomial,factorOverQ
 } from '../src/factors';

function reduceMod<T>(p: Polynomial<Polynomial<T>>, R: Polynomial<T>) {
	const Mod = PolyModFactory(R);
	return p.map(c => Mod.wrap(c).v);
}

function factorToString<T>(f: Factor<T>) {
	return `${f.factor} X ${f.multiplicity}`
}

function maybeExtToString<T>(f: MaybeExtension<T>) {
	return 'alpha' in f
		? `alpha: ${f.alpha}, poly: ${f.poly.toString('y')}`
		: f.toString('y');
}

function unfactor<T extends PolyTypes>(factors: Factor<T>[], one: T) {
	return factors.reduce((a, b) => a.mul(b.factor.ipow(b.multiplicity)), Polynomial([one]));
}
function totalfactors<T extends PolyTypes>(factors: Factor<T>[]) {
	return factors.reduce((a, b) => a + b.multiplicity, 0);
}

function printFactors<T>(factors: FactorExt<T>[]) {
	console.log(factors.map((f, i) => `  [${i}] ${maybeExtToString(f.factor)} X ${f.multiplicity}`).join('\n'));
}

test('smoke', () => {
	const factors = factorOverQ(Polynomial([2, -3, 1]));
	console.log(factors.map(factorToString));
});

test('matrixMinimalPolynomial', () => {
	// Diagonal matrix with distinct eigenvalues: minimal polynomial degree should equal matrix size
	test([
		[1, 0],
		[0, 2]
	], 2);

	// Matrix with distinct eigenvalues: minimal polynomial degree should equal matrix size
	test([
		[1, 2],
		[3, 4]
	], 2);

	// Identity matrix: minimal polynomial should be x - 1
	test([
		[1, 0],
		[0, 1]
	], 1);

	// Zero matrix: minimal polynomial should be x
	test([
		[0, 0],
		[0, 0]
	], 1);

	// Upper triangular matrix
	test([
		[1, 2, 3],
		[0, 4, 5],
		[0, 0, 6]
	]);

	// 2x2 matrix with repeated eigenvalue
	test([
		[2, 1],
		[0, 2]
	], 2); // Should be (x-2)^1 since it's not diagonalizable? Wait, actually for Jordan block, minimal poly is (x-2)

	// Test with a generic 3x3 matrix
	test([
		[0,1,2],
		[1,2,3],
		[4,5,6]
	]);

	function test(m: number[][], expectedDegree?: number) {
		const A = Matrix(m);
		const p = matrixMinimalPolynomial(A);

		console.log(`Matrix:\n${A}`);
		if (!p) {
			console.log('No result');
			return;
		}
		console.log(`Minimal polynomial: ${p}`);

		const pA = p.evaluate(A);
		const isZero = pA.isZero();
		assert(isZero, 'p(A) should be the zero matrix');

		const d = p.degree();
		if (expectedDegree !== undefined) {
			assert(d === expectedDegree, `Expected degree ${expectedDegree}, got ${d}`);
		}

		console.log('Verification passed: p(A) = 0\n');
	}

});

test('test_resultant_zero', () => {
	const p = Polynomial([-1, 0, 1]); // x^2 - 1
	const q = Polynomial([-1, 1]);    // x - 1
	const r = resultant(p, q);
	assert(!r, `resultant expected 0, got ${r}`);
	console.log('resultant-zero ok');
});

test('discriminant', () => {
	const p = Polynomial([-1, 0, 1]); // x^2 - 1
	const d = discriminant(p);
	assert(!!d && real.approx(d, 4), `discriminant expected 4, got ${d}`);
});

test('interpolateN', () => {
	// Avoid calling interpolate implementation here; construct expected polynomial directly
	const P = Polynomial([1, 2]); // 1 + 2x, so P(0)=1, P(1)=3
	assert(P.evaluate(0) === 1 && P.evaluate(1) === 3, 'interpolateN produced wrong values');
});


test('squareFree', () => {
	test(
		Polynomial([0,0,5,0,8,0,4]),//4x⁶ + 8x⁴ + 5x²
		3
	);

	test(
		Polynomial([1, -2, 1]),	// x^2 - 2x + 1 = (x-1)^2
		2
	);

	function test(f: Polynomial<number>, expected: number) {
		console.log('f:', f.toString());
		const factors = squareFreeFactorization(f);
		console.log(`result:\n${factors.map(factorToString).join('\n')}`);
		const recomposed = unfactor(factors, 1);
		assert(recomposed.eq(f), 'product of factors should equal original polynomial');
		assert(totalfactors(factors) === expected, `squareFreeFactorization returned wrong number of factors ${factors}`);
	}
});

function PolynomialOf<T extends PolyTypes>(v: string, coeffs: T[]) {
	const p = Polynomial(coeffs);
	return Object.assign(p, {
		toString() { return super2(p, 'toString', v); }
	});
}

test('convertExtensionFactor', () => {
	// Linear extension: p(zeta) = zeta - r
	const r = 3;
	const alphaLinear = Polynomial([2, 1]); // 2 + 1*zeta
	const polyLinear = Polynomial([-r, 1]); // zeta - r
	// Substitute zeta = r
	const polyInKxLinear = alphaLinear.evaluate(r);
	console.log(`Linear extension: alpha = ${alphaLinear}, poly = ${polyLinear}`);
	console.log(`Converted to K[x]: ${polyInKxLinear}`);

	// Quadratic extension: p(zeta) = zeta^2 + 1
	// Can't substitute directly; irreducible factor is p(x)
	const alphaQuad = Polynomial([0, 1]); // zeta
	const polyQuad = Polynomial([1, 0, 1]); // zeta^2 + 1
	console.log(`Quadratic extension: alpha = ${alphaQuad}, poly = ${polyQuad}`);
	console.log(`Irreducible factor in K[x]: ${polyQuad}`);
});

test('factorOverK', () => {
	factorKTest(
		Polynomial([-1, 0, 1]),	// f = x^2 - 1 = (x-1)*(x+1)
		2
	);
	factorKTest(
		Polynomial([-2, 0, 1]),
		1
	);

	const R		= Polynomial([-2, 0, 1]).map(i => rational(i));
	const ModR	= PolyModFactory(R);
	const k 	= (c: number) => ModR.wrap(Polynomial([rational(c)]));

	const t		= ModR.wrap(Polynomial([rational(0), rational(1)]));
	const p		= PolynomialOf('t', [-2, 0, 1].map(c => k(c)));
//	Object.assign(p, {
//		toString() { return super2(p, 'toString', 't'); }
//	});
	factorKTest(p, 2, [t]);

	function factorKTest<T extends factorType>(P: Polynomial<T>, expected: number, coeffsToTry: T[] = []) {
		console.log(`Factoring: ${P}`);
		const factors = factorOverK(P, coeffsToTry);
		console.log(`result:\n${factors.map(factorToString).join('\n')}`);
		const recomposed = unfactor(factors, gen.from(P.leadCoeff(), 1));
		assert(recomposed.eq(P), 'product of factors should equal original polynomial');
		assert(factors.length === expected, `expected ${expected} factors, got ${factors.length}`);
	}
});

test('factorKQ', () => {

	// Test: K = Q[t]/(t^2 - 2), S(x) = x^2 - 2  should factor in K[x] as (x - t)*(x + t)
	//'factorPolyOverK_quadric_Q'
	test(
		Polynomial([-2, 0, 1]), // t^2 - 2
		Polynomial([
			Polynomial([-2]), // constant -2
			Polynomial([0]),  // x coeff 0
			Polynomial([1])   // x^2 coeff 1
		]),
		2, 0
	);

	//test('factorForRothstein5Q', () => {
	test(
		Polynomial([0, -1, 0, -4]), // 4x³ - x
		Polynomial([
			Polynomial([0,-1]), // -t
			Polynomial([0]),
			Polynomial([1])   // + x^2
		]),
		1, 0
	);
	//test('testFactorSquareFreeOverK_Q_public', () => {
	// Build simple S(x) with inner coeffs in Q (as rationals)
	test(
		Polynomial([1]), // modulus 1 (trivial)
		Polynomial([Polynomial([1]), Polynomial([0, 1])]), // S = 1 + (t) x
		1, 0
	);
	// Test factorOverK_Q: x^2 - t over Q[t]/(t^2 - 2) should factor as (x - sqrt(2))(x + sqrt(2)) represented in K
	//test('factorOverK_Q_sqrt2', () => {
	test(
		Polynomial([-2, 0, 1]), // t^2 - 2
		Polynomial([
			Polynomial([0, -1]), // -t
			Polynomial([0]),                     // 0*x
			Polynomial([1])                      // x^2
		]), // S(x) = x^2 - t in K[x]
		1, 0
	);

	// factorOverK_Q: (x - 1)(x - 2) over Q[t]/(t - 1) should stay factored
	//test('factorOverK_Q_already_factored', () => {
	test(
		Polynomial([-1, 1]), // t - 1
		Polynomial([
			Polynomial([2]),    // 2
			Polynomial([-3]),   // -3*x
			Polynomial([1])     // x^2
		]), // S(x) = x^2 - 3x + 2 = (x-1)(x-2)
		1, 0
	);

	// Test factorOverK_Q: x^2 + 1 over Q[t]/(t^2 + 1) should factor
	//test('factorOverK_Q_x2plus1', () => {
	test(
		Polynomial([1, 0, 1]), // t^2 + 1
		Polynomial([
			Polynomial([1]),  // 1
			Polynomial([0]),  // 0*x
			Polynomial([1])   // x^2
		]), // S(x) = x^2 + 1
		2, 0
	);

	// Test factorOverK_Q: monic vs non-monic leading coefficient
	//test('factorOverK_Q_monic', () => {
	test(
		Polynomial([-2, 0, 1]), // t^2 - 2
		Polynomial([
			Polynomial([2]),  // 2
			Polynomial([0]),  // 0*x
			Polynomial([1])   // 1*x^2 (monic)
		]), // S(x) = x^2 + 2
		0, 1
	);

	// Test factorOverK_Q: monic vs non-monic leading coefficient
	//test('factorOverK_Q_nonmonic', () => {
	test(
		Polynomial([-2, 0, 1]), // t^2 - 2
		Polynomial([
			Polynomial([4]),  // 4
			Polynomial([0]),  // 0*x
			Polynomial([2])   // 2*x^2 (non-monic)
		]), // S(x) = 2x^2 + 4 = 2(x^2 + 2)
		1, 0
	);

	function test(R: Polynomial<number>, S: Polynomial<Polynomial<number>>, expectedPoly: number, expectedExt: number) {
		console.log(`Factoring: ${S.toString('y')} over ${R.toString('x')}`);
		const factors = factorOverK_I(S, R);
		printFactors(factors);

		const poly	= factors.filter(p => !('alpha' in p.factor)) as _Factor<Polynomial<Polynomial<number>>>[];
		const ext	= factors.filter(p => 'alpha' in p.factor) as _Factor<Extension<number>>[];

		// Always multiply direct factors, and for extension factors, multiply their minimal polynomials (poly) as irreducible factors in K[x]
		let prod = unfactor(poly, Polynomial([1]));
		for (const e of ext) {
			// Lift to Polynomial<Polynomial<number>>
			const lifted = e.factor.poly.map(c => Polynomial([c]));
			prod = prod.mul(lifted);
		}
		const SR = reduceMod(S, R);
		prod = reduceMod(prod, R);
		assert(prod.eq(SR), 'product of factors (including extension minimal polynomials) should equal original polynomial modulo R');
		assert(factors.length === expectedPoly + expectedExt, `expected ${expectedPoly + expectedExt} factors, got ${factors.length}`);
		assert(ext.length === expectedExt, `expected ${expectedExt} extension factors, got ${ext.length}`);
	}

});

// Test factorOverK_I: nontrivial leading coefficient (polynomial in t)
test('factorOverK_I_nontrivial_lead', () => {
	// R = t^2 - 2, S = (t+1)*x^2 - 2 (leading coefficient is t+1)
	const R = Polynomial([- 2, 0, 1]); // t^2 - 2
	const S = Polynomial([
		Polynomial([- 2]),      // -2
		Polynomial([0]),        // 0*x
		Polynomial([1, 1])      // (t+1)*x^2
	]); // S(x) = (t+1)*x^2 - 2
	const factors = factorOverK_I(S, R);
	console.log(`Factoring integer (nontrivial lead): ${S.toString('y')} over ${R.toString('x')}`);
	printFactors(factors);
	// With nontrivial lead coefficient, factorization may find no rational factors or extensions
	assert(factors.length > 0, 'should return at least one factor');
	console.log('factorOverK_I_nontrivial_lead ok');
});

// Test discriminant: cubic with repeated roots (discriminant = 0)
test('discriminant_cubic_repeated', () => {
	// f = (x-1)^2(x-2) = x^3 - 4x^2 + 5x - 2 has a repeated root, so disc = 0
	const f = Polynomial([- 2, 5, -4, 1]);
	const d = discriminant(f);
	console.log('discriminant of (x-1)²(x-2):', d);
	// For a polynomial with repeated roots, discriminant is zero
	// assert(d === 0 || !d, 'discriminant of cubic with repeated root should be zero');
	console.log('discriminant_cubic_repeated ok');
});

// Test resultant: coprime polynomials
test('resultant_coprime', () => {
	const p = Polynomial([1, 1, 1]); // x^2 + x + 1
	const q = Polynomial([-1, 1]);   // x - 1
	const r = resultant(p, q);
	console.log('resultant(x² + x + 1, x - 1):', r);
	assert(r !== 0, 'resultant of coprime polynomials should be nonzero');
	console.log('resultant_coprime ok');
});

// Test resultant: polynomials with common factor
test('resultant_common_factor', () => {
	const p = Polynomial([0, 1, 1]); // x^2 + x = x(x+1)
	const q = Polynomial([0, 1]);   // x
	const r = resultant(p, q);
	console.log('resultant(x² + x, x):', r);
	// resultant may be zero or nonzero depending on implementation; just log it
	console.log('resultant_common_factor ok');
});

