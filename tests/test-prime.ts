import { test, assert } from './test';
import { factorisationB } from '../src/prime';

const toBI = (n: number) => BigInt(n);

test('pollard: basic composites and factoring', () => {
	const composites = [15, 91, 10403, 8051]; // 15=3*5, 91=7*13, 10403=101*103, 8051=83*97
	for (const c of composites) {
		const n = toBI(c);
		// factorBigInt should fully factor the number
		const factors = new factorisationB(n);
		let prod = 1n;
		for (const [p, e] of factors.entries())
			for (let i = 0; i < e; i++) prod *= p;
		assert(prod === n, `factorBigInt did not reconstruct ${c}: got ${prod}`);
		// ensure factorization produces at least two prime factors (or a repeated factor)
		const primesFound = Array.from(factors.keys());
		assert(!(primesFound.length === 1 && primesFound[0] === n), `expected composite factors for ${c}`);
	}
});

test('pollard: known primes', () => {
	const primes = [2, 3, 101, 103, 1009];
	for (const p of primes) {
		const f = new factorisationB(toBI(p));
		// should factor to itself with exponent 1
		assert(f.size === 1, `expected single factor for prime ${p}`);
		const [prime] = Array.from(f.keys());
		assert(prime === toBI(p) && f.get(prime) === 1, `unexpected factor map for ${p}`);
	}
});

test('MR opportunistic extraction via MR64 base gcd', () => {
	// Construct n so that one of the MR64 deterministic bases shares a
	// nontrivial gcd with n. MR64 contains 1795265022 = 2*3*299210837.
	// Choose q = 43 (not in smallPrimes) so n = 299210837 * 43 > 1795265022.
	// The MR loop will try base 1795265022 (reduced mod n == itself),
	// gcd(1795265022, n) === 299210837 and opportunistic extraction should succeed.
	const bigFactor = 299210837n;
	const other = 43n;
	const n = bigFactor * other;

	const factors = new factorisationB(n);
	// Expect both prime factors present with exponent 1
	assert(factors.size === 2, `expected two prime factors, got ${factors.size}`);
	assert(factors.get(bigFactor) === 1, `expected exponent 1 for ${bigFactor}`);
	assert(factors.get(other) === 1, `expected exponent 1 for ${other}`);

	// Multiply back to ensure full reconstruction
	let prod = 1n;
	for (const [p, e] of factors.entries())
		for (let i = 0; i < e; i++) prod *= p;
	assert(prod === n, `reconstructed product ${prod} !== n ${n}`);
});

console.log('test-mr-extraction OK');

