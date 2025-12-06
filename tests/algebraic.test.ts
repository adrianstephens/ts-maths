import { algebraicOps } from '../src/algebraic';
import rational from '../src/rational';
import { test, expect } from './test';

test('basic arithmetic', () => {
	const a = algebraicOps.from(2);
	const b = algebraicOps.from(3);
	
	expect(algebraicOps.add(a, b), 'add numbers').toEqual(5);
	expect(algebraicOps.mul(a, b), 'multiply numbers').toEqual(6);
});

test('radical operations', () => {
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	
	// √2 * √2 = 2
	const result1 = algebraicOps.mul(sqrt2, sqrt2);
	expect(result1, 'sqrt2 * sqrt2').toEqual(2);
});

test('rational arithmetic', () => {
	const half = rational(1, 2);
	const third = rational(1, 3);
	
	const sum = algebraicOps.add(half, third);
	expect(sum, 'rational sum numerator').toEqual(rational(5,6));
});

test('power operations', () => {
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	
	// (√2)^2 = 2
	const squared = algebraicOps.ipow(sqrt2, 2);
	expect(squared, 'power operation').toEqual(2);
});

test('comprehensive algebraic simplification', () => {
	// Test: (√8 + √18 - √2) × (√2 + 1/2) = ?
	// Expected: √8 = 2√2, √18 = 3√2, so (2√2 + 3√2 - √2) × (√2 + 1/2) = 4√2 × (√2 + 1/2) = 8 + 2√2
	
	const sqrt8 = algebraicOps.pow(8, rational(1, 2));   // Should become 2√2
	const sqrt18 = algebraicOps.pow(18, rational(1, 2)); // Should become 3√2
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	const half = rational(1, 2);
	
	// First sum: √8 + √18 - √2 = 2√2 + 3√2 - √2 = 4√2
	const sum1 = algebraicOps.add(algebraicOps.add(sqrt8, sqrt18), algebraicOps.neg(sqrt2));
	
	// Second sum: √2 + 1/2
	const sum2 = algebraicOps.add(sqrt2, half);
	
	// Product: 4√2 × (√2 + 1/2) = 4√2 × √2 + 4√2 × 1/2 = 8 + 2√2
	const result = algebraicOps.mul(sum1, sum2);
	
	// Expected: 8 + 2√2
	const expected = algebraicOps.add(8, algebraicOps.mul(2, sqrt2));
	
	expect(result, 'comprehensive simplification').toEqual(expected);
});

test('nested radicals and zero elimination', () => {
	// Test: √(√16) × 0 + (∛8)^3 - 2^3 = ?
	// Expected: √(√16) = √4 = 2, so 2×0 + 8 - 8 = 0
	
	const sqrt16 = algebraicOps.pow(16, rational(1, 2)); // = 4
	const nestedRoot = algebraicOps.pow(sqrt16, rational(1, 2)); // √4 = 2
	
	const cubeRoot8 = algebraicOps.pow(8, rational(1, 3)); // ∛8 = 2
	const cubed = algebraicOps.ipow(cubeRoot8, 3); // 2^3 = 8
	
	// nestedRoot × 0 should eliminate to 0
	const zeroTerm = algebraicOps.mul(nestedRoot, 0);
	
	// Final: 0 + 8 - 8 = 0
	const result = algebraicOps.add(algebraicOps.add(zeroTerm, cubed), algebraicOps.neg(8));
	
	expect(result, 'nested radicals with zero').toEqual(0);
});

test('rational exponents with radical combination', () => {
	// Test: (2^(3/4) × 8^(1/4)) × (√2 × √8) = ?
	// Expected: 2^(3/4) × 2^(3/4) × √2 × 2√2 = 2^(3/2) × 4 = 2√2 × 4 = 8√2
	
	const term1 = algebraicOps.pow(2, rational(3, 4));
	const term2 = algebraicOps.pow(8, rational(1, 4)); // = 2^(3/4)
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	const sqrt8 = algebraicOps.pow(8, rational(1, 2)); // = 2√2
	
	const product1 = algebraicOps.mul(term1, term2); // Should combine exponents to 2^(3/2) = 2√2
	const product2 = algebraicOps.mul(sqrt2, sqrt8);  // Should combine to √16 = 4
	
	const result = algebraicOps.mul(product1, product2);
	
	// Expected: 2√2 × 4 = 8√2
	const expected = algebraicOps.mul(8, sqrt2);
	expect(result, 'rational exponents with radicals').toEqual(expected);
});

test('complex radical simplifications', () => {
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	const sqrt3 = algebraicOps.pow(3, rational(1, 2));
	const sqrt6 = algebraicOps.pow(6, rational(1, 2));
	
	// √2 * √3 = √6
	const product = algebraicOps.mul(sqrt2, sqrt3);
	expect(product, '√2 * √3 = √6').toEqual(sqrt6);
	
	// √8 = 2√2
	const sqrt8 = algebraicOps.pow(8, rational(1, 2));
	const twoSqrt2 = algebraicOps.mul(2, sqrt2);
	expect(sqrt8, '√8 = 2√2').toEqual(twoSqrt2);
});

test('nested radical operations', () => {
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	
	// ∜2 = 2^(1/4)
	const fourthRoot2 = algebraicOps.pow(2, rational(1, 4));
	
	// (∜2)^2 = √2
	const squared = algebraicOps.ipow(fourthRoot2, 2);
	expect(squared, '(∜2)^2 = √2').toEqual(sqrt2);
	
	// √(√2) = ∜2
	const nestedRoot = algebraicOps.pow(sqrt2, rational(1, 2));
	expect(nestedRoot, '√(√2) = ∜2').toEqual(fourthRoot2);
});

test('rational exponent simplifications', () => {
	// 8^(2/3) = (8^(1/3))^2 = 2^2 = 4
	const result1 = algebraicOps.pow(8, rational(2, 3));
	expect(result1, '8^(2/3) = 4').toEqual(4);
	
	// 27^(1/3) = 3
	const result2 = algebraicOps.pow(27, rational(1, 3));
	expect(result2, '27^(1/3) = 3').toEqual(3);
	
	// 16^(3/4) = (16^(1/4))^3 = 2^3 = 8
	const result3 = algebraicOps.pow(16, rational(3, 4));
	expect(result3, '16^(3/4) = 8').toEqual(8);
});

test('mixed rational and radical operations', () => {
	const half = rational(1, 2);
	const sqrt2 = algebraicOps.pow(2, rational(1, 2));
	
	// (1/2) * √2 = √2/2
	const product = algebraicOps.mul(half, sqrt2);
	const expected = algebraicOps.div(sqrt2, 2);
	expect(product, '(1/2) * √2 = √2/2').toEqual(expected);
	
	// √2 + √2 = 2√2
	const sum = algebraicOps.add(sqrt2, sqrt2);
	const twoSqrt2 = algebraicOps.mul(2, sqrt2);
	expect(sum, '√2 + √2 = 2√2').toEqual(twoSqrt2);
});