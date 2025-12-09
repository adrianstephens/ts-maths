import algebraic from '../src/algebraic';
import { Type } from '../src/core';
import rational from '../src/rational';
import { Polynomial } from '../src/polynomial';
import { complexT } from '../src/complex';
import { test, expect } from './test';

function sqrt(x: algebraic) {
	return algebraic.pow(x, rational(1, 2));
}
function cbrt(x: algebraic) {
	return algebraic.pow(x, rational(1, 3));
}

test('basic arithmetic', () => {
	const a = algebraic.from(2);
	const b = algebraic.from(3);
	
	expect(algebraic.add(a, b), 'add numbers').toEqual(5);
	expect(algebraic.mul(a, b), 'multiply numbers').toEqual(6);
});

test('radical operations', () => {
	const sqrt2 = sqrt(2);
	
	// √2 * √2 = 2
	const result1 = algebraic.mul(sqrt2, sqrt2);
	expect(result1, 'sqrt2 * sqrt2').toEqual(2);
});

test('rational arithmetic', () => {
	const half = rational(1, 2);
	const third = rational(1, 3);
	
	const sum = algebraic.add(half, third);
	expect(sum, 'rational sum numerator').toEqual(rational(5,6));
});

test('power operations', () => {
	const sqrt2 = sqrt(2);
	
	// (√2)^2 = 2
	const squared = algebraic.ipow(sqrt2, 2);
	expect(squared, 'power operation').toEqual(2);
});

test('comprehensive algebraic simplification', () => {
	// Test: (√8 + √18 - √2) × (√2 + 1/2) = ?
	// Expected: √8 = 2√2, √18 = 3√2, so (2√2 + 3√2 - √2) × (√2 + 1/2) = 4√2 × (√2 + 1/2) = 8 + 2√2
	
	const sqrt8 = sqrt(8);   // Should become 2√2
	const sqrt18 = sqrt(18); // Should become 3√2
	const sqrt2 = sqrt(2);
	const half = rational(1, 2);
	
	// First sum: √8 + √18 - √2 = 2√2 + 3√2 - √2 = 4√2
	const sum1 = algebraic.add(algebraic.add(sqrt8, sqrt18), algebraic.neg(sqrt2));
	
	// Second sum: √2 + 1/2
	const sum2 = algebraic.add(sqrt2, half);
	
	// Product: 4√2 × (√2 + 1/2) = 4√2 × √2 + 4√2 × 1/2 = 8 + 2√2
	const result = algebraic.mul(sum1, sum2);
	
	// Expected: 8 + 2√2
	const expected = algebraic.add(8, algebraic.mul(2, sqrt2));
	
	expect(result, 'comprehensive simplification').toEqual(expected);

	const base = algebraic.add(sqrt2, 1);
	console.log(String(algebraic.pow(base, 4)));

	//(2+√3)^(1/3) + (2-√3)^(1/3) = 1
	//(a+b√c)^(1/3) + (a-b√c)^(1/3)
	//a = 2, b = 1, c = 5	=> (2+√5)^(1/3)+(2-√5)^(1/3)

	const term1 = algebraic.pow(algebraic.add(2, sqrt(5)), rational(1, 3));
	const term2 = algebraic.pow(algebraic.sub(2, sqrt(5)), rational(1, 3));
	const sumTerms = algebraic.add(term1, term2);
	expect(sumTerms, 'nested radicals').toEqual(1);

	//√(5+2√6) = √3 + √2
	const value = algebraic.pow(algebraic.add(algebraic.mul(sqrt(6), 2), 5), rational(1, 2));
	expect(value, 'nested radicals').toEqual(algebraic.add(sqrt(3), sqrt(2)));
});

test('nested radicals and zero elimination', () => {
	// Test: √(√16) × 0 + (∛8)^3 - 2^3 = ?
	// Expected: √(√16) = √4 = 2, so 2×0 + 8 - 8 = 0
	
	const sqrt16 = sqrt(16); // = 4
	const nestedRoot = sqrt(sqrt16); // √4 = 2
	
	const cubeRoot8 = cbrt(8); // ∛8 = 2
	const cubed = algebraic.ipow(cubeRoot8, 3); // 2^3 = 8
	
	// nestedRoot × 0 should eliminate to 0
	const zeroTerm = algebraic.mul(nestedRoot, 0);
	
	// Final: 0 + 8 - 8 = 0
	const result = algebraic.add(algebraic.add(zeroTerm, cubed), algebraic.neg(8));
	
	expect(result, 'nested radicals with zero').toEqual(0);
});

test('rational exponents with radical combination', () => {
	// Test: (2^(3/4) × 8^(1/4)) × (√2 × √8) = ?
	// Expected: 2^(3/4) × 2^(3/4) × √2 × 2√2 = 2^(3/2) × 4 = 2√2 × 4 = 8√2
	
	const term1 = algebraic.pow(2, rational(3, 4));
	const term2 = algebraic.pow(8, rational(1, 4)); // = 2^(3/4)
	const sqrt2 = sqrt(2);
	const sqrt8 = sqrt(8); // = 2√2
	
	const product1 = algebraic.mul(term1, term2); // Should combine exponents to 2^(3/2) = 2√2
	const product2 = algebraic.mul(sqrt2, sqrt8);  // Should combine to √16 = 4
	
	const result = algebraic.mul(product1, product2);
	
	// Expected: 2√2 × 4 = 8√2
	const expected = algebraic.mul(8, sqrt2);
	expect(result, 'rational exponents with radicals').toEqual(expected);
});

test('complex radical simplifications', () => {
	const sqrt2 = sqrt(2);
	const sqrt3 = sqrt(3);
	const sqrt6 = sqrt(6);
	
	// √2 * √3 = √6
	const product = algebraic.mul(sqrt2, sqrt3);
	expect(product, '√2 * √3 = √6').toEqual(sqrt6);
	
	// √8 = 2√2
	const sqrt8 = sqrt(8);
	const twoSqrt2 = algebraic.mul(2, sqrt2);
	expect(sqrt8, '√8 = 2√2').toEqual(twoSqrt2);
});

test('nested radical operations', () => {
	const sqrt2 = sqrt(2);
	
	// ∜2 = 2^(1/4)
	const fourthRoot2 = algebraic.pow(2, rational(1, 4));
	
	// (∜2)^2 = √2
	const squared = algebraic.ipow(fourthRoot2, 2);
	expect(squared, '(∜2)^2 = √2').toEqual(sqrt2);
	
	// √(√2) = ∜2
	const nestedRoot = sqrt(sqrt2);
	expect(nestedRoot, '√(√2) = ∜2').toEqual(fourthRoot2);
});

test('rational exponent simplifications', () => {
	// 8^(2/3) = (8^(1/3))^2 = 2^2 = 4
	const result1 = algebraic.pow(8, rational(2, 3));
	expect(result1, '8^(2/3) = 4').toEqual(4);
	
	// 27^(1/3) = 3
	const result2 = cbrt(27);
	expect(result2, '27^(1/3) = 3').toEqual(3);
	
	// 16^(3/4) = (16^(1/4))^3 = 2^3 = 8
	const result3 = algebraic.pow(16, rational(3, 4));
	expect(result3, '16^(3/4) = 8').toEqual(8);
});

test('mixed rational and radical operations', () => {
	const half = rational(1, 2);
	const sqrt2 = sqrt(2);
	
	// (1/2) * √2 = √2/2
	const product = algebraic.mul(half, sqrt2);
	const expected = algebraic.div(sqrt2, 2);
	expect(product, '(1/2) * √2 = √2/2').toEqual(expected);
	
	// √2 + √2 = 2√2
	const sum = algebraic.add(sqrt2, sqrt2);
	const twoSqrt2 = algebraic.mul(2, sqrt2);
	expect(sum, '√2 + √2 = 2√2').toEqual(twoSqrt2);
});

test('type adaptor', () => {
	const alg = Type(algebraic);

	const a = new alg(sqrt(2));
	const b = a.add(a.div(a));

	console.log(String(b));

	const one = new alg(algebraic.from(1));
	
	// Test denesting: m + 1/m where m = ∛(√2+1)
	const sqrt2_plus_1 = algebraic.add(sqrt(2), 1);
	const m = algebraic.pow(sqrt2_plus_1, rational(1, 3));
	const m_plus_inv = algebraic.add(m, algebraic.div(1, m));
	console.log('m + 1/m =', String(m_plus_inv));

	const x = new alg(sqrt2_plus_1);
	const x1 = x.rpow(1, 3);
	const x2 = x1.mul(x1);
	const x3 = x2.mul(x1);
	
	const c = new complexT(one, one);
	const r = c.cbrt();
	console.log('cbrt(1+i) =', String(r.r), '+', String(r.i), 'i');
	// Verify: r^3 should equal 1+i
	const r3 = r.mul(r).mul(r);
	console.log('(cbrt(1+i))^3 =', String(r3.r), '+', String(r3.i), 'i');
	expect(r3.r, 'real part').toEqual(one);
	expect(r3.i, 'imag part').toEqual(one);

	let poly = Polynomial([new alg(algebraic.from(1))]);

	for (let j = 1; j < 20; ++j) {
		poly = poly.mul(Polynomial([new alg(algebraic.from(-j)), new alg(algebraic.from(1))]));
		console.log(String(poly));
		const r1 = poly.realRoots();
		console.log(r1.map(String));
		const r0 = poly.rationalRoots();
		console.log(r0);
	}

});
